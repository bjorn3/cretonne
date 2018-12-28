//! Precomputed constant hash tables.
//!
//! The `lib/codegen/meta-python/constant_hash.py` Python module can generate constant hash tables
//! using open addressing and quadratic probing. The hash tables are arrays that are guaranteed to:
//!
//! - Have a power-of-two size.
//! - Contain at least one empty slot.

/// A primitive hash function for matching opcodes.
/// Must match `lib/codegen/meta-python/constant_hash.py`.
pub fn simple_hash(s: &str) -> usize {
    let mut h: u32 = 5381;
    for c in s.chars() {
        h = (h ^ c as u32).wrapping_add(h.rotate_right(6));
    }
    h as usize
}

/// Compute an open addressed, quadratically probed hash table containing
/// `items`. The returned table is a list containing the elements of the
/// iterable `items` and `None` in unused slots.
pub fn generate_table<T, H: Fn(&T) -> usize>(items: &Vec<T>, hash_function: H) -> Vec<Option<&T>> {
    let size = (1.20 * items.len() as f64) as usize;
    // TODO do we really need the multiply by two here?
    let size = if size.is_power_of_two() {
        size * 2
    } else {
        size.next_power_of_two()
    };

    let mut table: Vec<Option<&T>> = Vec::new();
    table.resize(size, None);

    for i in items {
        let mut h = hash_function(i) % size;
        let mut s = 0;
        while table[h].is_some() {
            s += 1;
            h = (h + s) % size;
        }
        table[h] = Some(i);
    }

    table
}

/// Trait that must be implemented by the entries in a constant hash table.
pub trait Table<K: Copy + Eq> {
    /// Get the number of entries in this table which must be a power of two.
    fn len(&self) -> usize;

    /// Get the key corresponding to the entry at `idx`, or `None` if the entry is empty.
    /// The `idx` must be in range.
    fn key(&self, idx: usize) -> Option<K>;
}

/// Look for `key` in `table`.
///
/// The provided `hash` value must have been computed from `key` using the same hash function that
/// was used to construct the table.
///
/// Returns `Ok(idx)` with the table index containing the found entry, or `Err(idx)` with the empty
/// sentinel entry if no entry could be found.
pub fn probe<K: Copy + Eq, T: Table<K> + ?Sized>(
    table: &T,
    key: K,
    hash: usize,
) -> Result<usize, usize> {
    debug_assert!(table.len().is_power_of_two());
    let mask = table.len() - 1;

    let mut idx = hash;
    let mut step = 0;

    loop {
        idx &= mask;

        match table.key(idx) {
            None => return Err(idx),
            Some(k) if k == key => return Ok(idx),
            _ => {}
        }

        // Quadratic probing.
        step += 1;
        // When `table.len()` is a power of two, it can be proven that `idx` will visit all
        // entries. This means that this loop will always terminate if the hash table has even
        // one unused entry.
        debug_assert!(step < table.len());
        idx += step;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn basic() {
        // c.f. `meta-python/constant_hash.py` tests.
        assert_eq!(simple_hash("Hello"), 0x2fa70c01);
        assert_eq!(simple_hash("world"), 0x5b0c31d5);
    }

    #[test]
    fn test_generate_table() {
        let v = vec!["Hello".to_string(), "world".to_string()];
        let table = generate_table(&v, |s| simple_hash(&s));
        assert_eq!(
            table,
            vec![
                None,
                Some(&"Hello".to_string()),
                Some(&"world".to_string()),
                None
            ]
        );
    }
}
