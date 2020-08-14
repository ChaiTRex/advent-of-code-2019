use crate::memory::Memory;
use std::{error::Error, fs::File, io::Read, iter::FromIterator, path::Path};

pub type MachineWord = i64;

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Machine {
    memory: Memory,
    instruction_pointer: MachineWord,
    relative_base: MachineWord,
}

impl Machine {
    #[inline(always)]
    pub fn new(memory: Memory) -> Machine {
        Machine {
            memory,
            instruction_pointer: 0,
            relative_base: 0,
        }
    }

    pub fn from_str(code: &str) -> Result<Machine, Box<dyn Error>> {
        let mut memory = Memory::new();
        for (i, line) in (0..).zip(code.split(',')) {
            memory[i] = line.trim().parse()?;
        }

        Ok(Machine::new(memory))
    }

    pub fn from_file<P: AsRef<Path>>(filepath: P) -> Result<Machine, Box<dyn Error>> {
        let mut file = File::open(filepath)?;
        let mut contents = String::new();
        file.read_to_string(&mut contents)?;

        Machine::from_str(&contents)
    }

    pub fn execute(mut self) -> Result<MachinePause, MachineError> {
        match self.execute_internal() {
            Ok(x) => Ok(match x {
                UnfinishedMachinePause::AwaitingInput(destination) => {
                    MachinePause::AwaitingInput(MachineAwaitingInput {
                        machine: self,
                        destination,
                    })
                }
                UnfinishedMachinePause::HasOutput(output) => MachinePause::HasOutput(self, output),
                UnfinishedMachinePause::Halted => {
                    MachinePause::Halted(HaltedMachine { machine: self })
                }
            }),
            Err(x) => Err(match x {
                UnfinishedMachineError::InvalidInstruction(message) => {
                    MachineError::InvalidInstruction(
                        message,
                        self.instruction_pointer,
                        self.memory[self.instruction_pointer],
                        MachineWithError { machine: self },
                    )
                }
            }),
        }
    }

    #[inline(always)]
    fn execute_internal(&mut self) -> Result<UnfinishedMachinePause, UnfinishedMachineError> {
        loop {
            let opcode = self.memory[self.instruction_pointer];
            if opcode < 0 {
                return Err(UnfinishedMachineError::InvalidInstruction(
                    "negative opcode",
                ));
            }
            let base_opcode = opcode % 100;
            let addressing_modes = opcode / 100;
            match base_opcode {
                1 => {
                    let (m1, m2, m3) = Self::three_addressing_modes(addressing_modes)?;
                    let a = self.get_value(m1, 1)?;
                    let b = self.get_value(m2, 2)?;
                    self.set_value(m3, 3, a + b)?;
                    self.instruction_pointer += 4;
                }
                2 => {
                    let (m1, m2, m3) = Self::three_addressing_modes(addressing_modes)?;
                    let a = self.get_value(m1, 1)?;
                    let b = self.get_value(m2, 2)?;
                    self.set_value(m3, 3, a * b)?;
                    self.instruction_pointer += 4;
                }
                3 => {
                    let m1 = Self::one_addressing_mode(addressing_modes)?;
                    let destination = self.get_write_address(m1, 1)?;
                    self.instruction_pointer += 2;
                    return Ok(UnfinishedMachinePause::AwaitingInput(destination));
                }
                4 => {
                    let m1 = Self::one_addressing_mode(addressing_modes)?;
                    let output = self.get_value(m1, 1)?;
                    self.instruction_pointer += 2;
                    return Ok(UnfinishedMachinePause::HasOutput(output));
                }
                5 => {
                    let (m1, m2) = Self::two_addressing_modes(addressing_modes)?;
                    if self.get_value(m1, 1)? != 0 {
                        self.instruction_pointer = self.get_value(m2, 2)?;
                    } else {
                        self.instruction_pointer += 3;
                    }
                }
                6 => {
                    let (m1, m2) = Self::two_addressing_modes(addressing_modes)?;
                    if self.get_value(m1, 1)? == 0 {
                        self.instruction_pointer = self.get_value(m2, 2)?;
                    } else {
                        self.instruction_pointer += 3;
                    }
                }
                7 => {
                    let (m1, m2, m3) = Self::three_addressing_modes(addressing_modes)?;
                    let a = self.get_value(m1, 1)?;
                    let b = self.get_value(m2, 2)?;
                    self.set_value(m3, 3, if a < b { 1 } else { 0 })?;
                    self.instruction_pointer += 4;
                }
                8 => {
                    let (m1, m2, m3) = Self::three_addressing_modes(addressing_modes)?;
                    let a = self.get_value(m1, 1)?;
                    let b = self.get_value(m2, 2)?;
                    self.set_value(m3, 3, if a == b { 1 } else { 0 })?;
                    self.instruction_pointer += 4;
                }
                9 => {
                    let m1 = Self::one_addressing_mode(addressing_modes)?;
                    self.relative_base += self.get_value(m1, 1)?;
                    self.instruction_pointer += 2;
                }
                99 => {
                    Self::zero_addressing_modes(addressing_modes)?;
                    return Ok(UnfinishedMachinePause::Halted);
                }
                _ => {
                    return Err(UnfinishedMachineError::InvalidInstruction(
                        "unknown base opcode (last two digits should be 01 - 09 or 99)",
                    ))
                }
            }
        }
    }

    #[inline(always)]
    fn zero_addressing_modes(addressing_modes: MachineWord) -> Result<(), UnfinishedMachineError> {
        if addressing_modes >= 10 {
            Err(UnfinishedMachineError::InvalidInstruction(
                "too many addressing mode digits",
            ))
        } else {
            Ok(())
        }
    }

    #[inline(always)]
    fn one_addressing_mode(
        addressing_modes: MachineWord,
    ) -> Result<MachineWord, UnfinishedMachineError> {
        if addressing_modes >= 10 {
            Err(UnfinishedMachineError::InvalidInstruction(
                "too many addressing mode digits",
            ))
        } else {
            Ok(addressing_modes)
        }
    }

    #[inline(always)]
    fn two_addressing_modes(
        addressing_modes: MachineWord,
    ) -> Result<(MachineWord, MachineWord), UnfinishedMachineError> {
        if addressing_modes >= 100 {
            Err(UnfinishedMachineError::InvalidInstruction(
                "too many addressing mode digits",
            ))
        } else {
            let a = addressing_modes % 10;
            let b = addressing_modes / 10;
            Ok((a, b))
        }
    }

    #[inline(always)]
    fn three_addressing_modes(
        addressing_modes: MachineWord,
    ) -> Result<(MachineWord, MachineWord, MachineWord), UnfinishedMachineError> {
        if addressing_modes >= 1000 {
            Err(UnfinishedMachineError::InvalidInstruction(
                "too many addressing mode digits",
            ))
        } else {
            let a = addressing_modes % 10;
            let addressing_modes = addressing_modes / 10;
            let b = addressing_modes % 10;
            let c = addressing_modes / 10;
            Ok((a, b, c))
        }
    }

    fn get_value(
        &self,
        addressing_mode: MachineWord,
        offset: MachineWord,
    ) -> Result<MachineWord, UnfinishedMachineError> {
        match addressing_mode {
            0 => Ok(self.memory[self.memory[self.instruction_pointer + offset]]),
            1 => Ok(self.memory[self.instruction_pointer + offset]),
            2 => Ok(
                self.memory[self.relative_base + self.memory[self.instruction_pointer + offset]]
            ),
            _ => Err(UnfinishedMachineError::InvalidInstruction(
                "invalid addressing mode for reading (addressing mode digit should be 0, 1, or 2)",
            )),
        }
    }

    #[inline(always)]
    fn set_value(
        &mut self,
        addressing_mode: MachineWord,
        offset: MachineWord,
        value: MachineWord,
    ) -> Result<(), UnfinishedMachineError> {
        let write_address = self.get_write_address(addressing_mode, offset)?;
        self.memory[write_address] = value;
        Ok(())
    }

    #[inline(always)]
    fn get_write_address(
        &self,
        addressing_mode: MachineWord,
        offset: MachineWord,
    ) -> Result<MachineWord, UnfinishedMachineError> {
        Ok(match addressing_mode {
            0 => self.memory[self.instruction_pointer + offset],
            2 => self.relative_base + self.memory[self.instruction_pointer + offset],
            _ => {
                return Err(UnfinishedMachineError::InvalidInstruction(
                    "invalid addressing mode for writing (addressing mode digit should be 0 or 2)",
                ));
            }
        })
    }

    #[inline(always)]
    pub fn memory(&self) -> &Memory {
        &self.memory
    }

    #[inline(always)]
    pub fn memory_mut(&mut self) -> &mut Memory {
        &mut self.memory
    }

    #[inline(always)]
    pub fn instruction_pointer(&self) -> MachineWord {
        self.instruction_pointer
    }

    #[inline(always)]
    pub fn instruction_pointer_mut(&mut self) -> &mut MachineWord {
        &mut self.instruction_pointer
    }

    #[inline(always)]
    pub fn relative_base(&self) -> MachineWord {
        self.relative_base
    }

    #[inline(always)]
    pub fn relative_base_mut(&mut self) -> &mut MachineWord {
        &mut self.relative_base
    }
}

impl FromIterator<MachineWord> for Machine {
    #[inline(always)]
    fn from_iter<I: IntoIterator<Item = MachineWord>>(iter: I) -> Self {
        Machine::new(Memory::from_iter(iter))
    }
}

impl<'a> FromIterator<&'a MachineWord> for Machine {
    #[inline(always)]
    fn from_iter<I: IntoIterator<Item = &'a MachineWord>>(iter: I) -> Self {
        Machine::new(Memory::from_iter(iter))
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct MachineAwaitingInput {
    machine: Machine,
    destination: MachineWord,
}

impl MachineAwaitingInput {
    pub fn push_input(mut self, input: MachineWord) -> Machine {
        self.machine.memory[self.destination] = input;
        self.machine
    }

    #[inline(always)]
    pub fn memory(&self) -> &Memory {
        self.machine.memory()
    }

    #[inline(always)]
    pub fn memory_mut(&mut self) -> &mut Memory {
        self.machine.memory_mut()
    }

    #[inline(always)]
    pub fn instruction_pointer(&self) -> MachineWord {
        self.machine.instruction_pointer()
    }

    #[inline(always)]
    pub fn instruction_pointer_mut(&mut self) -> &mut MachineWord {
        self.machine.instruction_pointer_mut()
    }

    #[inline(always)]
    pub fn relative_base(&self) -> MachineWord {
        self.machine.relative_base()
    }

    #[inline(always)]
    pub fn relative_base_mut(&mut self) -> &mut MachineWord {
        self.machine.relative_base_mut()
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct HaltedMachine {
    machine: Machine,
}

impl HaltedMachine {
    #[inline(always)]
    pub fn memory(&self) -> &Memory {
        self.machine.memory()
    }

    #[inline(always)]
    pub fn memory_mut(&mut self) -> &mut Memory {
        self.machine.memory_mut()
    }

    #[inline(always)]
    pub fn instruction_pointer(&self) -> MachineWord {
        self.machine.instruction_pointer()
    }

    #[inline(always)]
    pub fn instruction_pointer_mut(&mut self) -> &mut MachineWord {
        self.machine.instruction_pointer_mut()
    }

    #[inline(always)]
    pub fn relative_base(&self) -> MachineWord {
        self.machine.relative_base()
    }

    #[inline(always)]
    pub fn relative_base_mut(&mut self) -> &mut MachineWord {
        self.machine.relative_base_mut()
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct MachineWithError {
    machine: Machine,
}

impl MachineWithError {
    #[inline(always)]
    pub fn memory(&self) -> &Memory {
        self.machine.memory()
    }

    #[inline(always)]
    pub fn memory_mut(&mut self) -> &mut Memory {
        self.machine.memory_mut()
    }

    #[inline(always)]
    pub fn instruction_pointer(&self) -> MachineWord {
        self.machine.instruction_pointer()
    }

    #[inline(always)]
    pub fn instruction_pointer_mut(&mut self) -> &mut MachineWord {
        self.machine.instruction_pointer_mut()
    }

    #[inline(always)]
    pub fn relative_base(&self) -> MachineWord {
        self.machine.relative_base()
    }

    #[inline(always)]
    pub fn relative_base_mut(&mut self) -> &mut MachineWord {
        self.machine.relative_base_mut()
    }
}

enum UnfinishedMachinePause {
    AwaitingInput(MachineWord),
    HasOutput(MachineWord),
    Halted,
}

pub enum MachinePause {
    AwaitingInput(MachineAwaitingInput),
    HasOutput(Machine, MachineWord),
    Halted(HaltedMachine),
}

enum UnfinishedMachineError {
    InvalidInstruction(&'static str),
}

#[derive(Debug)]
pub enum MachineError {
    InvalidInstruction(&'static str, MachineWord, MachineWord, MachineWithError),
}
