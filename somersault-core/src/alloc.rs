use std::default;

#[derive(Debug)]
struct Chunk {
    index: usize,
    size: usize,
    is_free: bool,
}

pub struct MemoryAllocator {
    chunks: Vec<Chunk>,
}

impl default::Default for MemoryAllocator {
    fn default() -> Self {
        Self::new(0x7FFF)
    }
}

impl MemoryAllocator {
    pub fn new(total_size: usize) -> Self {
        let initial_chunk = Chunk {
            index: 0,
            size: total_size,
            is_free: true,
        };

        Self {
            chunks: vec![initial_chunk],
        }
    }

    pub fn allocate(&mut self, size: usize) -> Option<usize> {
        // Find first free chunk that fits
        if let Some(chunk_idx) = self
            .chunks
            .iter()
            .position(|chunk| chunk.is_free && chunk.size >= size)
        {
            let chunk = &mut self.chunks[chunk_idx];
            chunk.is_free = false;
            let index = chunk.index;

            // Split chunk if it's larger than needed
            if chunk.size > size {
                let new_chunk = Chunk {
                    index: chunk.index + size,
                    size: chunk.size - size,
                    is_free: true,
                };
                chunk.size = size;
                self.chunks.insert(chunk_idx + 1, new_chunk);
            }

            Some(index)
        } else {
            None
        }
    }

    pub fn deallocate(&mut self, index: usize) {
        if let Some(chunk_idx) = self.chunks.iter().position(|chunk| chunk.index == index) {
            self.chunks[chunk_idx].is_free = true;
            self.merge_free_chunks();
        }
    }

    fn merge_free_chunks(&mut self) {
        let mut i = 0;
        while i < self.chunks.len() - 1 {
            if self.chunks[i].is_free && self.chunks[i + 1].is_free {
                let next_chunk = self.chunks.remove(i + 1);
                self.chunks[i].size += next_chunk.size;
            } else {
                i += 1;
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_allocate() {
        let mut allocator = MemoryAllocator::default();
        assert_eq!(allocator.allocate(4), Some(0));
        assert_eq!(allocator.allocate(4), Some(4));
        assert_eq!(allocator.allocate(4), Some(8));

        allocator.deallocate(4);
        assert_eq!(allocator.allocate(4), Some(4));

        allocator.deallocate(8);
        allocator.deallocate(0);

        assert_eq!(allocator.allocate(4), Some(0));
    }
}
