use crate::machine::MachineWord;
use std::{
    collections::{HashMap, HashSet},
    fmt::{self, Debug, Formatter},
    iter::FromIterator,
    ops::{Index, IndexMut},
};

// If you want to make the MemoryLines bigger, change this. MemoryLine will then hold
// 2^MEMORY_LINE_ADDRESS_BITS MachineWords.
const MEMORY_LINE_ADDRESS_BITS: usize = 10;

// Don't change these. They should be always derived from MEMORY_LINE_ADDRESS_BITS.
const MEMORY_LINE_LENGTH: usize = 1 << MEMORY_LINE_ADDRESS_BITS;
const MEMORY_LINE_BITMASK: MachineWord = (1 << MEMORY_LINE_ADDRESS_BITS) - 1;

#[derive(Clone, Debug)]
pub struct Memory {
    line_zero: MemoryLine,
    lines: HashMap<MachineWord, MemoryLine>,
}

impl Memory {
    pub fn new() -> Memory {
        Memory {
            line_zero: Default::default(),
            lines: HashMap::new(),
        }
    }
}

impl Eq for Memory {}

impl FromIterator<MachineWord> for Memory {
    fn from_iter<I: IntoIterator<Item = MachineWord>>(iter: I) -> Self {
        let mut memory = Memory::new();

        for (index, element) in (0..).zip(iter) {
            memory[index] = element;
        }

        memory
    }
}

impl<'a> FromIterator<&'a MachineWord> for Memory {
    #[inline(always)]
    fn from_iter<I: IntoIterator<Item = &'a MachineWord>>(iter: I) -> Self {
        FromIterator::from_iter(iter.into_iter().cloned())
    }
}

impl Index<MachineWord> for Memory {
    type Output = MachineWord;

    fn index(&self, i: MachineWord) -> &Self::Output {
        let address_high_bits = i >> MEMORY_LINE_ADDRESS_BITS;
        let address_low_bits = (i & MEMORY_LINE_BITMASK) as usize;

        if address_high_bits == 0 {
            self.line_zero.0.index(address_low_bits)
        } else {
            self.lines
                .get(&address_high_bits)
                .map(|line| line.0.index(address_low_bits))
                .unwrap_or(&0)
        }
    }
}

impl IndexMut<MachineWord> for Memory {
    fn index_mut(&mut self, i: MachineWord) -> &mut Self::Output {
        let address_high_bits = i >> MEMORY_LINE_ADDRESS_BITS;
        let address_low_bits = (i & MEMORY_LINE_BITMASK) as usize;

        if address_high_bits == 0 {
            self.line_zero.0.index_mut(address_low_bits)
        } else {
            self.lines
                .entry(address_high_bits)
                .or_default()
                .0
                .index_mut(address_low_bits)
        }
    }
}

const MEMORY_LINE_OF_ZEROS: MemoryLine = MemoryLine([0; MEMORY_LINE_LENGTH]);

impl PartialEq for Memory {
    fn eq(&self, other: &Self) -> bool {
        self.line_zero == other.line_zero
            && self
                .lines
                .keys()
                .chain(other.lines.keys())
                .collect::<HashSet<_>>()
                .iter()
                .all(|i| match (self.lines.get(i), other.lines.get(i)) {
                    (Some(xs), Some(ys)) => xs == ys,
                    (Some(xs), None) => *xs == MEMORY_LINE_OF_ZEROS,
                    (None, Some(ys)) => MEMORY_LINE_OF_ZEROS == *ys,
                    (None, None) => unreachable!(),
                })
    }
}

#[derive(Clone)]
#[repr(align(64))]
struct MemoryLine([MachineWord; MEMORY_LINE_LENGTH]);

impl Debug for MemoryLine {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        Debug::fmt(&self.0[..], f)
    }
}

impl Default for MemoryLine {
    fn default() -> Self {
        MemoryLine([0; MEMORY_LINE_LENGTH])
    }
}

impl Eq for MemoryLine {}

impl PartialEq for MemoryLine {
    fn eq(&self, other: &Self) -> bool {
        self.0.iter().zip(other.0.iter()).all(|(a, b)| a == b)
    }
}
