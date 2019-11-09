//! Defines `ObjectBackend`.

use crate::traps::{ObjectTrapSink, ObjectTrapSite};
use cranelift_codegen::binemit::{
    Addend, CodeOffset, NullStackmapSink, NullTrapSink, Reloc, RelocSink,
};
use cranelift_codegen::entity::SecondaryMap;
use cranelift_codegen::isa::TargetIsa;
use cranelift_codegen::{self, binemit, ir};
use cranelift_module::{
    Backend, DataContext, DataDescription, DataId, FuncId, Init, Linkage, ModuleNamespace,
    ModuleResult,
};
use object::write::{Object, Relocation, SectionId, StandardSection, Symbol, SymbolId};
use object::{RelocationEncoding, RelocationKind, SymbolKind, SymbolScope};
use std::collections::HashMap;
use target_lexicon::PointerWidth;

#[derive(Debug)]
/// Setting to enable collection of traps. Setting this to `Enabled` in
/// `ObjectBuilder` means that `ObjectProduct` will contains trap sites.
pub enum ObjectTrapCollection {
    /// `ObjectProduct::traps` will be empty
    Disabled,
    /// `ObjectProduct::traps` will contain trap sites
    Enabled,
}

/// A builder for `ObjectBackend`.
pub struct ObjectBuilder {
    isa: Box<dyn TargetIsa>,
    name: String,
    collect_traps: ObjectTrapCollection,
    libcall_names: Box<dyn Fn(ir::LibCall) -> String>,
    function_alignment: u64,
}

impl ObjectBuilder {
    /// Create a new `ObjectBuilder` using the given Cranelift target, that
    /// can be passed to [`Module::new`](cranelift_module::Module::new).
    ///
    /// `collect_traps` setting determines whether trap information is collected in the
    /// `ObjectProduct`.
    ///
    /// The `libcall_names` function provides a way to translate `cranelift_codegen`'s `ir::LibCall`
    /// enum to symbols. LibCalls are inserted in the IR as part of the legalization for certain
    /// floating point instructions, and for stack probes. If you don't know what to use for this
    /// argument, use `cranelift_module::default_libcall_names()`.
    pub fn new(
        isa: Box<dyn TargetIsa>,
        name: String,
        collect_traps: ObjectTrapCollection,
        libcall_names: Box<dyn Fn(ir::LibCall) -> String>,
    ) -> ModuleResult<Self> {
        Ok(Self {
            isa,
            name,
            collect_traps,
            libcall_names,
            function_alignment: 1,
        })
    }

    /// Set the alignment used for functions.
    pub fn function_alignment(&mut self, alignment: u64) -> &mut Self {
        self.function_alignment = alignment;
        self
    }
}

/// A `ObjectBackend` implements `Backend` and emits ".o" files using the `object` library.
///
/// See the `ObjectBuilder` for a convenient way to construct `ObjectBackend` instances.
pub struct ObjectBackend {
    isa: Box<dyn TargetIsa>,
    object: Object,
    functions: SecondaryMap<FuncId, Option<SymbolId>>,
    data_objects: SecondaryMap<DataId, Option<SymbolId>>,
    traps: SecondaryMap<FuncId, Vec<ObjectTrapSite>>,
    libcalls: HashMap<ir::LibCall, SymbolId>,
    libcall_names: Box<dyn Fn(ir::LibCall) -> String>,
    collect_traps: ObjectTrapCollection,
    function_alignment: u64,
    frame_sink: Option<FrameSink>,
}

impl Backend for ObjectBackend {
    type Builder = ObjectBuilder;

    type CompiledFunction = ObjectCompiledFunction;
    type CompiledData = ObjectCompiledData;

    // There's no need to return individual artifacts; we're writing them into
    // the output file instead.
    type FinalizedFunction = ();
    type FinalizedData = ();

    type Product = ObjectProduct;

    /// Create a new `ObjectBackend` using the given Cranelift target.
    fn new(builder: ObjectBuilder) -> Self {
        let triple = builder.isa.triple();
        let mut object = Object::new(triple.binary_format, triple.architecture);
        object.add_file_symbol(builder.name.as_bytes().to_vec());
        Self {
            isa: builder.isa,
            object,
            functions: SecondaryMap::new(),
            data_objects: SecondaryMap::new(),
            traps: SecondaryMap::new(),
            libcalls: HashMap::new(),
            libcall_names: builder.libcall_names,
            collect_traps: builder.collect_traps,
            function_alignment: builder.function_alignment,
            frame_sink: Some(FrameSink::new()),
        }
    }

    fn isa(&self) -> &dyn TargetIsa {
        &*self.isa
    }

    fn declare_function(&mut self, id: FuncId, name: &str, linkage: Linkage) {
        let (scope, weak) = translate_linkage(linkage);

        if let Some(function) = self.functions[id] {
            let symbol = self.object.symbol_mut(function);
            symbol.scope = scope;
            symbol.weak = weak;
        } else {
            let symbol_id = self.object.add_symbol(Symbol {
                name: name.as_bytes().to_vec(),
                value: 0,
                size: 0,
                kind: SymbolKind::Text,
                scope,
                weak,
                section: None,
            });
            self.functions[id] = Some(symbol_id);
        }
    }

    fn declare_data(
        &mut self,
        id: DataId,
        name: &str,
        linkage: Linkage,
        _writable: bool,
        _align: Option<u8>,
    ) {
        let (scope, weak) = translate_linkage(linkage);

        if let Some(data) = self.data_objects[id] {
            let symbol = self.object.symbol_mut(data);
            symbol.scope = scope;
            symbol.weak = weak;
        } else {
            let symbol_id = self.object.add_symbol(Symbol {
                name: name.as_bytes().to_vec(),
                value: 0,
                size: 0,
                kind: SymbolKind::Data,
                scope,
                weak,
                section: None,
            });
            self.data_objects[id] = Some(symbol_id);
        }
    }

    fn define_function(
        &mut self,
        func_id: FuncId,
        _name: &str,
        ctx: &cranelift_codegen::Context,
        _namespace: &ModuleNamespace<Self>,
        code_size: u32,
    ) -> ModuleResult<ObjectCompiledFunction> {
        let mut code: Vec<u8> = vec![0; code_size as usize];
        let mut reloc_sink = ObjectRelocSink::default();
        let mut trap_sink = ObjectTrapSink::default();
        let mut stackmap_sink = NullStackmapSink {};

        if let ObjectTrapCollection::Enabled = self.collect_traps {
            unsafe {
                ctx.emit_to_memory(
                    &*self.isa,
                    code.as_mut_ptr(),
                    &mut reloc_sink,
                    &mut trap_sink,
                    &mut stackmap_sink,
                )
            };
        } else {
            let mut trap_sink = NullTrapSink {};
            unsafe {
                ctx.emit_to_memory(
                    &*self.isa,
                    code.as_mut_ptr(),
                    &mut reloc_sink,
                    &mut trap_sink,
                    &mut stackmap_sink,
                )
            };
        }

        let symbol = self.functions[func_id].unwrap();
        let section = self.object.section_id(StandardSection::Text);
        let offset = self
            .object
            .add_symbol_data(symbol, section, &code, self.function_alignment);
        self.traps[func_id] = trap_sink.sites;

        let code_length = code.len() as u32;

        if let Some(ref mut frame_sink) = self.frame_sink {
            if let Some(layout) = ctx.func.frame_layout.as_ref() {
                let reg_mapper = DwarfRegMapper::for_isa(&self.isa);

                let (cie, mut encoder) = frame_sink.cie_for(&layout.initial, &reg_mapper);

                let mut fd_entry =
                    FrameDescriptionEntry::new(frame_sink.address_for(symbol), code_length);

                let mut frame_changes = vec![];
                for ebb in ctx.func.layout.ebbs() {
                    for (offset, inst, size) in
                        ctx.func.inst_offsets(ebb, &self.isa.encoding_info())
                    {
                        if let Some(changes) = layout.instructions.get(&inst) {
                            for change in changes.iter() {
                                frame_changes.push((offset + size, change.clone()));
                            }
                        }
                    }
                }

                frame_changes.sort_by(|a, b| a.0.cmp(&b.0));

                let fde_insts = frame_changes
                    .into_iter()
                    .flat_map(|(addr, change)| encoder.translate(&change).map(|inst| (addr, inst)));

                for (addr, inst) in fde_insts.into_iter() {
                    fd_entry.add_instruction(addr, inst);
                }

                frame_sink.add_fde(cie, fd_entry);
            } else {
                // we have a frame sink to write .eh_frames into, but are not collecting debug
                // information for at least the current function. This might be a bug in the code
                // using cranelift-faerie?
            }
        }

        Ok(ObjectCompiledFunction {
            offset,
            size: code_size,
            section,
            relocs: reloc_sink.relocs,
        })
    }

    fn define_data(
        &mut self,
        data_id: DataId,
        _name: &str,
        writable: bool,
        align: Option<u8>,
        data_ctx: &DataContext,
        _namespace: &ModuleNamespace<Self>,
    ) -> ModuleResult<ObjectCompiledData> {
        let &DataDescription {
            ref init,
            ref function_decls,
            ref data_decls,
            ref function_relocs,
            ref data_relocs,
        } = data_ctx.description();

        let size = init.size();
        let mut data = Vec::with_capacity(size);
        match *init {
            Init::Uninitialized => {
                panic!("data is not initialized yet");
            }
            Init::Zeros { .. } => {
                data.resize(size, 0);
            }
            Init::Bytes { ref contents } => {
                data.extend_from_slice(contents);
            }
        }

        let reloc_size = match self.isa.triple().pointer_width().unwrap() {
            PointerWidth::U16 => 16,
            PointerWidth::U32 => 32,
            PointerWidth::U64 => 64,
        };
        let mut relocs = Vec::new();
        for &(offset, id) in function_relocs {
            relocs.push(RelocRecord {
                offset,
                name: function_decls[id].clone(),
                kind: RelocationKind::Absolute,
                encoding: RelocationEncoding::Generic,
                size: reloc_size,
                addend: 0,
            });
        }
        for &(offset, id, addend) in data_relocs {
            relocs.push(RelocRecord {
                offset,
                name: data_decls[id].clone(),
                kind: RelocationKind::Absolute,
                encoding: RelocationEncoding::Generic,
                size: reloc_size,
                addend,
            });
        }

        let symbol = self.data_objects[data_id].unwrap();
        let section = self.object.section_id(if writable {
            StandardSection::Data
        } else if relocs.is_empty() {
            StandardSection::ReadOnlyData
        } else {
            StandardSection::ReadOnlyDataWithRel
        });
        let offset =
            self.object
                .add_symbol_data(symbol, section, &data, u64::from(align.unwrap_or(1)));
        Ok(ObjectCompiledData {
            offset,
            section,
            relocs,
        })
    }

    fn write_data_funcaddr(
        &mut self,
        _data: &mut ObjectCompiledData,
        _offset: usize,
        _what: ir::FuncRef,
    ) {
        unimplemented!()
    }

    fn write_data_dataaddr(
        &mut self,
        _data: &mut ObjectCompiledData,
        _offset: usize,
        _what: ir::GlobalValue,
        _usize: binemit::Addend,
    ) {
        unimplemented!()
    }

    fn finalize_function(
        &mut self,
        _id: FuncId,
        func: &ObjectCompiledFunction,
        namespace: &ModuleNamespace<Self>,
    ) {
        for &RelocRecord {
            offset,
            ref name,
            kind,
            encoding,
            size,
            addend,
        } in &func.relocs
        {
            let offset = func.offset + u64::from(offset);
            let symbol = self.get_symbol(namespace, name);
            self.object
                .add_relocation(
                    func.section,
                    Relocation {
                        offset,
                        size,
                        kind,
                        encoding,
                        symbol,
                        addend,
                    },
                )
                .unwrap();
        }
    }

    fn get_finalized_function(&self, _func: &ObjectCompiledFunction) {
        // Nothing to do.
    }

    fn finalize_data(
        &mut self,
        _id: DataId,
        data: &ObjectCompiledData,
        namespace: &ModuleNamespace<Self>,
    ) {
        for &RelocRecord {
            offset,
            ref name,
            kind,
            encoding,
            size,
            addend,
        } in &data.relocs
        {
            let offset = data.offset + u64::from(offset);
            let symbol = self.get_symbol(namespace, name);
            self.object
                .add_relocation(
                    data.section,
                    Relocation {
                        offset,
                        size,
                        kind,
                        encoding,
                        symbol,
                        addend,
                    },
                )
                .unwrap();
        }
    }

    fn get_finalized_data(&self, _data: &ObjectCompiledData) {
        // Nothing to do.
    }

    fn publish(&mut self) {
        // Nothing to do.
    }

    fn finish(mut self) -> ObjectProduct {
        if let Some(ref mut frame_sink) = self.frame_sink {
            let eh_frame = self.object.add_section(b"__TEXT".to_vec(), b"__eh_frame".to_vec(), object::SectionKind::Unknown);
            let eh_frame_sym = self.object.section_symbol(eh_frame);
            let mut eh_frame_bytes = Vec::new();
            let mut relocations = Vec::new();

            let mut eh_frame_writer = gimli::write::EhFrame(FaerieDebugSink {
                eh_frame,
                data: &mut eh_frame_bytes,
                functions: frame_sink.fn_names.as_slice(),
                object: &mut self.object,
                relocations: &mut relocations,
            });
            frame_sink
                .table
                .write_eh_frame(&mut eh_frame_writer)
                .unwrap();


            self.object.add_symbol_data(eh_frame_sym, eh_frame, &eh_frame_bytes, 8);

            for reloc in relocations {
                self.object.add_relocation(eh_frame, reloc).unwrap();
            }

            self.object.add_symbol(Symbol {
                name: b"__keep_eh_frame".to_vec(),
                value: 0,
                size: 0,
                kind: SymbolKind::Text,
                scope: SymbolScope::Dynamic,
                weak: false,
                section: Some(eh_frame),
            });
        }

        ObjectProduct {
            object: self.object,
            functions: self.functions,
            data_objects: self.data_objects,
            traps: self.traps,
        }
    }
}

impl ObjectBackend {
    // This should only be called during finalization because it creates
    // symbols for missing libcalls.
    fn get_symbol(
        &mut self,
        namespace: &ModuleNamespace<Self>,
        name: &ir::ExternalName,
    ) -> SymbolId {
        match *name {
            ir::ExternalName::User { .. } => {
                if namespace.is_function(name) {
                    let id = namespace.get_function_id(name);
                    self.functions[id].unwrap()
                } else {
                    let id = namespace.get_data_id(name);
                    self.data_objects[id].unwrap()
                }
            }
            ir::ExternalName::LibCall(ref libcall) => {
                let name = (self.libcall_names)(*libcall);
                if let Some(symbol) = self.object.symbol_id(name.as_bytes()) {
                    symbol
                } else if let Some(symbol) = self.libcalls.get(libcall) {
                    *symbol
                } else {
                    let symbol = self.object.add_symbol(Symbol {
                        name: name.as_bytes().to_vec(),
                        value: 0,
                        size: 0,
                        kind: SymbolKind::Text,
                        scope: SymbolScope::Unknown,
                        weak: false,
                        section: None,
                    });
                    self.libcalls.insert(*libcall, symbol);
                    symbol
                }
            }
            _ => panic!("invalid ExternalName {}", name),
        }
    }
}

fn translate_linkage(linkage: Linkage) -> (SymbolScope, bool) {
    let scope = match linkage {
        Linkage::Import => SymbolScope::Unknown,
        Linkage::Local => SymbolScope::Compilation,
        Linkage::Export | Linkage::Preemptible => SymbolScope::Dynamic,
    };
    // TODO: this matches rustc_codegen_cranelift, but may be wrong.
    let weak = linkage == Linkage::Preemptible;
    (scope, weak)
}

#[derive(Clone)]
pub struct ObjectCompiledFunction {
    offset: u64,
    size: u32,
    section: SectionId,
    relocs: Vec<RelocRecord>,
}

#[derive(Clone)]
pub struct ObjectCompiledData {
    offset: u64,
    section: SectionId,
    relocs: Vec<RelocRecord>,
}

/// This is the output of `Module`'s
/// [`finish`](../cranelift_module/struct.Module.html#method.finish) function.
/// It contains the generated `Object` and other information produced during
/// compilation.
pub struct ObjectProduct {
    /// Object artifact with all functions and data from the module defined.
    pub object: Object,
    /// Symbol IDs for functions (both declared and defined).
    pub functions: SecondaryMap<FuncId, Option<SymbolId>>,
    /// Symbol IDs for data objects (both declared and defined).
    pub data_objects: SecondaryMap<DataId, Option<SymbolId>>,
    /// Trap sites for defined functions.
    pub traps: SecondaryMap<FuncId, Vec<ObjectTrapSite>>,
}

impl ObjectProduct {
    /// Return the `SymbolId` for the given function.
    #[inline]
    pub fn function_symbol(&self, id: FuncId) -> SymbolId {
        self.functions[id].unwrap()
    }

    /// Return the `SymbolId` for the given data object.
    #[inline]
    pub fn data_symbol(&self, id: DataId) -> SymbolId {
        self.data_objects[id].unwrap()
    }

    /// Write the object bytes in memory.
    #[inline]
    pub fn emit(self) -> Result<Vec<u8>, String> {
        self.object.write()
    }
}

#[derive(Clone)]
struct RelocRecord {
    offset: CodeOffset,
    name: ir::ExternalName,
    kind: RelocationKind,
    encoding: RelocationEncoding,
    size: u8,
    addend: Addend,
}

#[derive(Default)]
struct ObjectRelocSink {
    relocs: Vec<RelocRecord>,
}

impl RelocSink for ObjectRelocSink {
    fn reloc_ebb(&mut self, _offset: CodeOffset, _reloc: Reloc, _ebb_offset: CodeOffset) {
        unimplemented!();
    }

    fn reloc_external(
        &mut self,
        offset: CodeOffset,
        reloc: Reloc,
        name: &ir::ExternalName,
        addend: Addend,
    ) {
        let (kind, encoding, size) = match reloc {
            Reloc::Abs4 => (RelocationKind::Absolute, RelocationEncoding::Generic, 32),
            Reloc::Abs8 => (RelocationKind::Absolute, RelocationEncoding::Generic, 64),
            Reloc::X86PCRel4 => (RelocationKind::Relative, RelocationEncoding::Generic, 32),
            Reloc::X86CallPCRel4 => (RelocationKind::Relative, RelocationEncoding::X86Branch, 32),
            // TODO: Get Cranelift to tell us when we can use
            // R_X86_64_GOTPCRELX/R_X86_64_REX_GOTPCRELX.
            Reloc::X86CallPLTRel4 => (
                RelocationKind::PltRelative,
                RelocationEncoding::X86Branch,
                32,
            ),
            Reloc::X86GOTPCRel4 => (RelocationKind::GotRelative, RelocationEncoding::Generic, 32),
            // FIXME
            _ => unimplemented!(),
        };
        self.relocs.push(RelocRecord {
            offset,
            name: name.clone(),
            kind,
            encoding,
            size,
            addend,
        });
    }

    fn reloc_jt(&mut self, _offset: CodeOffset, reloc: Reloc, _jt: ir::JumpTable) {
        match reloc {
            Reloc::X86PCRelRodata4 => {
                // Not necessary to record this unless we are going to split apart code and its
                // jumptbl/rodata.
            }
            _ => {
                panic!("Unhandled reloc");
            }
        }
    }

    fn reloc_constant(&mut self, _offset: CodeOffset, reloc: Reloc, _jt: ir::ConstantOffset) {
        match reloc {
            Reloc::X86PCRelRodata4 => {
                // Not necessary to record this unless we are going to split apart code and its
                // jumptbl/rodata.
            }
            _ => {
                panic!("Unhandled reloc");
            }
        }
    }
}

















use cranelift_codegen::isa::{RegInfo, RegUnit};
use cranelift_codegen::isa;
use cranelift_module::ModuleError;
use std::fs::File;
use target_lexicon::Triple;

use gimli::write::Address;
use gimli::write::CallFrameInstruction;
use gimli::write::CommonInformationEntry;
use gimli::write::FrameDescriptionEntry;

struct FrameSink {
    // we need to retain function names to hand out usize identifiers for FDE addresses,
    // which are then used to look up function names again for relocations, when `write_address` is
    // called.
    fn_names: Vec<SymbolId>,
    table: gimli::write::FrameTable,
}

impl FrameSink {
    /// Find a CIE appropriate for the register mapper and initial state provided. This will
    /// construct a CIE and rely on `gimli` to return an id for an appropriate existing CIE, if
    /// one exists.
    ///
    /// This function also returns a `CFIEncoder` already initialized to the state matching the
    /// initial CFI instructions for this CIE, ready for use to encode an FDE.
    pub fn cie_for<'a>(
        &mut self,
        initial_state: &[ir::FrameLayoutChange],
        reg_mapper: &'a DwarfRegMapper,
    ) -> (gimli::write::CieId, CFIEncoder<'a>) {
        let mut cie = CommonInformationEntry::new(
            gimli::Encoding {
                format: gimli::Format::Dwarf32,
                version: 1,
                address_size: 4,
            },
            // code alignment factor. Is this right for non-x86_64 ISAs? Probably could be 2 or 4
            // elsewhere.
            0x01,
            // data alignment factor. Same question for non-x86_64 ISAs.
            -0x08,
            // ISA-specific, column for the return address (may be a register, may not)
            reg_mapper.return_address(),
        );

        cie.fde_address_encoding = gimli::DwEhPe(0x1b);

        let mut encoder = CFIEncoder::new(&reg_mapper);

        for inst in initial_state
            .iter()
            .flat_map(|change| encoder.translate(change))
        {
            cie.add_instruction(inst);
        }

        let cie_id = self.table.add_cie(cie);

        (cie_id, encoder)
    }

    pub fn new() -> FrameSink {
        FrameSink {
            fn_names: vec![],
            table: gimli::write::FrameTable::default(),
        }
    }

    pub fn address_for(&mut self, symbol: SymbolId) -> Address {
        // adding a FrameDescriptionEntry for a function twice would be a bug,
        // so we can confidently expect that `name` will not be provided more than once.
        // So `name` is always new, meaning we can just add it and return its index
        self.fn_names.push(symbol);
        Address::Symbol {
            symbol: self.fn_names.len() - 1,
            addend: 0,
        }
    }

    /// Add a FrameDescriptionEntry to the FrameTable we're constructing
    ///
    /// This will always use the default CIE (which was build with this `FrameSink`).
    pub fn add_fde(&mut self, cie_id: gimli::write::CieId, fd_entry: FrameDescriptionEntry) {
        self.table.add_fde(cie_id, fd_entry);
    }
}

struct FaerieDebugSink<'a> {
    eh_frame: SectionId,
    pub data: &'a mut Vec<u8>,
    pub functions: &'a [SymbolId],
    pub object: &'a mut object::write::Object,
    relocations: &'a mut Vec<Relocation>,
}

impl<'a> gimli::write::Writer for FaerieDebugSink<'a> {
    type Endian = gimli::LittleEndian;

    fn endian(&self) -> Self::Endian {
        gimli::LittleEndian
    }
    fn len(&self) -> usize {
        self.data.len()
    }
    fn write(&mut self, bytes: &[u8]) -> gimli::write::Result<()> {
        self.data.extend_from_slice(bytes);
        Ok(())
    }

    fn write_at(&mut self, offset: usize, bytes: &[u8]) -> gimli::write::Result<()> {
        if offset + bytes.len() > self.data.len() {
            return Err(gimli::write::Error::LengthOutOfBounds);
        }
        self.data[offset..][..bytes.len()].copy_from_slice(bytes);
        Ok(())
    }

    fn write_eh_pointer(
        &mut self,
        address: Address,
        eh_pe: gimli::DwEhPe,
        size: u8,
    ) -> gimli::write::Result<()> {
        // we only support PC-relative 4byte signed offsets for eh_frame pointers currently. Other
        // encodings may be permissible, but aren't seen even by gcc/clang/etc, and have not been
        // tested. Currently, relocations used for addresses expect to be relocating four bytes,
        // PC-relative, and larger pointer sizes would require selection of other relocation types.
        assert!(eh_pe.0 == 0x1b);

        // if size is not 4, then the size indicated by `eh_pe` doesn't match with the pointer
        // we're trying to encode. That's a logical bug, possibly in gimli?
        assert!(size == 4);

        self.write_address(address, size)
    }

    fn write_address(&mut self, address: Address, size: u8) -> gimli::write::Result<()> {
        match address {
            Address::Constant(val) => self.write_udata(val, size),
            Address::Symbol { symbol, addend } => {
                assert!(addend == 0);

                let symbol = self.functions[symbol];

                let offset = self.data.len() as u64;

                self.write_udata(0, size)?;

                self.relocations.push(Relocation {
                        addend: 0,
                        encoding: RelocationEncoding::Generic,
                        kind: RelocationKind::Relative,
                        size: 32,
                        offset,
                        symbol,
                    });

                Ok(())
            }
        }
    }
}

struct CFIEncoder<'a> {
    reg_map: &'a DwarfRegMapper<'a>,
    cfa_def_reg: Option<isa::RegUnit>,
    cfa_def_offset: Option<isize>,
}

struct DwarfRegMapper<'a> {
    isa: &'a Box<dyn TargetIsa>,
    reg_info: RegInfo,
}

impl<'a> DwarfRegMapper<'a> {
    pub fn for_isa(isa: &'a Box<dyn TargetIsa>) -> Self {
        DwarfRegMapper {
            isa,
            reg_info: isa.register_info(),
        }
    }

    /// Translate a Cranelift `RegUnit` to its matching `Register` for DWARF use.
    ///
    /// panics if `reg` cannot be translated - the requested debug information would be
    /// unencodable.
    pub fn translate_reg(&self, reg: RegUnit) -> gimli::Register {
        match self.isa.name() {
            "x86" => {
                const X86_GP_REG_MAP: [u16; 16] = [
                    // cranelift rax == 0 -> dwarf rax == 0
                    0, // cranelift rcx == 1 -> dwarf rcx == 2
                    2, // cranelift rdx == 2 -> dwarf rdx == 1
                    1, // cranelift rbx == 3 -> dwarf rbx == 3
                    3, // cranelift rsp == 4 -> dwarf rsp == 7
                    7, // cranelift rbp == 5 -> dwarf rbp == 6
                    6, // cranelift rsi == 6 -> dwarf rsi == 4
                    4, // cranelift rdi == 7 -> dwarf rdi == 5
                    5, // all of r8 to r15 do map directly over
                    8, 9, 10, 11, 12, 13, 14, 15,
                ];
                let bank = self.reg_info.bank_containing_regunit(reg).unwrap();
                match bank.name {
                    "IntRegs" => {
                        // x86 GP registers have a weird mapping to DWARF registers, so we use a
                        // lookup table.
                        gimli::Register(X86_GP_REG_MAP[(reg - bank.first_unit) as usize])
                    }
                    "FloatRegs" => {
                        // xmm registers are all contiguous, but a bit offset
                        let xmm_num = reg - bank.first_unit;
                        // Cranelift only knows about sse4
                        assert!(xmm_num < 16);
                        gimli::Register(17 + xmm_num)
                    }
                    _ => {
                        panic!("unsupported register bank: {}", bank.name);
                    }
                }
            }
            /*
             * Other architectures, like "arm32", "arm64", and "riscv", do not have mappings to
             * DWARF register numbers yet.
             */
            name => {
                panic!("don't know how to encode registers for isa {}", name);
            }
        }
    }

    /// Get the DWARF location describing the call frame's return address.
    ///
    /// panics if that location is unknown - the requested debug information would be unencodable.
    pub fn return_address(&self) -> gimli::Register {
        match self.isa.name() {
            "x86" => gimli::Register(0x10),
            "arm32" => {
                // unlike AArch64, there is no explicit DWARF number for a return address
                // so trying to encode a return address in arm32 is a logical error.
                panic!("arm32 DWARF has no distinct return address register - this is a FrameChange bug, or you may want to have specified lr (r14)");
            }
            "arm64" => {
                // from "DWARF for the ARM 64-bit architecture (AArch64)"
                //
                // this is actually the "current mode exception link register". ARM uses LR for
                // return address purposes and CFI directives to preserve the parent call frame
                // should have been performed by preserving LR.
                gimli::Register(33)
            }
            "riscv" => {
                // Taking a guess from reading
                // https://github.com/riscv/riscv-elf-psabi-doc/blob/master/riscv-elf.md#dwarf-register-numbers
                //
                // which says dwarf number 64 is the "Alternate Frame Return Column", talking about
                // use for unwinding from signal handlers, recording the address the signal handler
                // will return to.
                gimli::Register(64)
            }
            name => {
                panic!("don't know how to encode registers for isa {}", name);
            }
        }
    }
}

impl<'a> CFIEncoder<'a> {
    pub fn new(reg_map: &'a DwarfRegMapper) -> Self {
        CFIEncoder {
            reg_map,
            // Both of the below are typically defined by per-CIE initial instructions, such that
            // neither are `None` when encoding instructions for an FDE. It is, however, likely not
            // an error for these to be `None` when encoding an FDE *AS LONG AS* they are
            // initialized before the CFI for an FDE advance into the function.
            cfa_def_reg: None,
            cfa_def_offset: None,
        }
    }

    pub fn translate(&mut self, change: &ir::FrameLayoutChange) -> Option<CallFrameInstruction> {
        match change {
            ir::FrameLayoutChange::CallFrameAddressAt { reg, offset } => {
                // if your call frame is more than 2gb, or -2gb.. sorry? I don't think .eh_frame
                // can express that? Maybe chaining `cfa_advance_loc4`, or something..
                assert_eq!(
                    *offset, *offset as i32 as isize,
                    "call frame offset beyond i32 range"
                );
                let (reg_updated, offset_updated) = (
                    Some(*reg) != self.cfa_def_reg,
                    Some(*offset) != self.cfa_def_offset,
                );
                self.cfa_def_offset = Some(*offset);
                self.cfa_def_reg = Some(*reg);
                match (reg_updated, offset_updated) {
                    (false, false) => {
                        /*
                         * this "change" would change nothing, so we don't have to
                         * do anything.
                         */
                        None
                    }
                    (true, false) => {
                        // reg pointing to the call frame has changed
                        Some(CallFrameInstruction::CfaRegister(
                            self.reg_map.translate_reg(*reg),
                        ))
                    }
                    (false, true) => {
                        // the offset has changed, so emit CfaOffset
                        Some(CallFrameInstruction::CfaOffset(*offset as i32))
                    }
                    (true, true) => {
                        // the register and cfa offset have changed, so update both
                        Some(CallFrameInstruction::Cfa(
                            self.reg_map.translate_reg(*reg),
                            *offset as i32,
                        ))
                    }
                }
            }
            ir::FrameLayoutChange::RegAt { reg, cfa_offset } => Some(CallFrameInstruction::Offset(
                self.reg_map.translate_reg(*reg),
                *cfa_offset as i32,
            )),
            ir::FrameLayoutChange::ReturnAddressAt { cfa_offset } => Some(
                CallFrameInstruction::Offset(self.reg_map.return_address(), *cfa_offset as i32),
            ),
            ir::FrameLayoutChange::Preserve => Some(CallFrameInstruction::RememberState),
            ir::FrameLayoutChange::Restore => Some(CallFrameInstruction::RestoreState),
        }
    }
}
