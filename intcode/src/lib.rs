pub mod machine;
pub mod memory;

#[cfg(test)]
mod tests {
    use self::ExpectedPause::*;
    use crate::machine::{
        Machine,
        MachineError::InvalidInstruction,
        MachinePause::{AwaitingInput, Halted, HasOutput},
        MachineWord,
    };
    use std::iter::once;

    #[test]
    fn parsing_works() {
        assert_eq!(
            Machine::from_str("1,5,1000,-6500").unwrap(),
            [1, 5, 1000, -6500].iter().collect()
        );
    }

    #[test]
    fn examples() {
        test(
            "day 2, example 0",
            &[1, 0, 0, 3, 99],
            &[HltWith(&[1, 0, 0, 2, 99])],
        );
        test(
            "day 2, example 1",
            &[1, 9, 10, 3, 2, 3, 11, 0, 99, 30, 40, 50],
            &[HltWith(&[3500, 9, 10, 70, 2, 3, 11, 0, 99, 30, 40, 50])],
        );
        test(
            "day 2, example 2",
            &[1, 0, 0, 0, 99],
            &[HltWith(&[2, 0, 0, 0, 99])],
        );
        test(
            "day 2, example 3",
            &[2, 3, 0, 3, 99],
            &[HltWith(&[2, 3, 0, 6, 99])],
        );
        test(
            "day 2, example 4",
            &[2, 4, 4, 5, 99, 0],
            &[HltWith(&[2, 4, 4, 5, 99, 9801])],
        );
        test(
            "day 2, example 5",
            &[1, 1, 1, 4, 99, 5, 6, 0, 99],
            &[HltWith(&[30, 1, 1, 4, 2, 5, 6, 0, 99])],
        );
        for i in -1000..=1000 {
            test(
                &format!("day 5, example 0, input {}", i),
                &[3, 0, 4, 0, 99],
                &[
                    InWith(i, &[i, 0, 4, 0, 99]),
                    OutWith(i, &[i, 0, 4, 0, 99]),
                    HltWith(&[i, 0, 4, 0, 99]),
                ],
            );
        }
        test(
            "day 5, example 1",
            &[1002, 4, 3, 4, 33],
            &[HltWith(&[1002, 4, 3, 4, 99])],
        );
        test(
            "day 5, example 2",
            &[1101, 100, -1, 4, 0],
            &[HltWith(&[1101, 100, -1, 4, 99])],
        );
        for i in -1000..=1000 {
            test(
                &format!("day 5, example 3, input {}", i),
                &[3, 9, 8, 9, 10, 9, 4, 9, 99, -1, 8],
                &[In(i), Out(if i == 8 { 1 } else { 0 }), Hlt],
            );
            test(
                &format!("day 5, example 4, input {}", i),
                &[3, 9, 7, 9, 10, 9, 4, 9, 99, -1, 8],
                &[In(i), Out(if i < 8 { 1 } else { 0 }), Hlt],
            );
            test(
                &format!("day 5, example 5, input {}", i),
                &[3, 3, 1108, -1, 8, 3, 4, 3, 99],
                &[In(i), Out(if i == 8 { 1 } else { 0 }), Hlt],
            );
            test(
                &format!("day 5, example 6, input {}", i),
                &[3, 3, 1107, -1, 8, 3, 4, 3, 99],
                &[In(i), Out(if i < 8 { 1 } else { 0 }), Hlt],
            );
            test(
                &format!("day 5, example 7, input {}", i),
                &[3, 12, 6, 12, 15, 1, 13, 14, 13, 4, 13, 99, -1, 0, 1, 9],
                &[In(i), Out(if i == 0 { 0 } else { 1 }), Hlt],
            );
            test(
                &format!("day 5, example 8, input {}", i),
                &[3, 3, 1105, -1, 9, 1101, 0, 0, 12, 4, 12, 99, 1],
                &[In(i), Out(if i == 0 { 0 } else { 1 }), Hlt],
            );
            test(
                &format!("day 5, example 9, input {}", i),
                &[
                    3, 21, 1008, 21, 8, 20, 1005, 20, 22, 107, 8, 21, 20, 1006, 20, 31, 1106, 0,
                    36, 98, 0, 0, 1002, 21, 125, 20, 4, 20, 1105, 1, 46, 104, 999, 1105, 1, 46,
                    1101, 1000, 1, 20, 4, 20, 1105, 1, 46, 98, 99,
                ],
                &[
                    In(i),
                    Out(if i < 8 {
                        999
                    } else if i == 8 {
                        1000
                    } else {
                        1001
                    }),
                    Hlt,
                ],
            );
        }
        test(
            "day 9, example 0",
            &[
                109, 1, 204, -1, 1001, 100, 1, 100, 1008, 100, 16, 101, 1006, 101, 0, 99,
            ],
            &[
                109, 1, 204, -1, 1001, 100, 1, 100, 1008, 100, 16, 101, 1006, 101, 0, 99,
            ]
            .iter()
            .cloned()
            .map(Out)
            .chain(once(Hlt))
            .collect::<Vec<_>>(),
        );
        test(
            "day 9, example 1",
            &[1102, 34915192, 34915192, 7, 4, 7, 99, 0],
            &[Out(1219070632396864), Hlt],
        );
        test(
            "day 9, example 2",
            &[104, 1125899906842624, 99],
            &[Out(1125899906842624), Hlt],
        );
    }

    enum ExpectedPause<'a> {
        In(MachineWord),
        InWith(MachineWord, &'a [MachineWord]),
        Out(MachineWord),
        OutWith(MachineWord, &'a [MachineWord]),
        Hlt,
        HltWith(&'a [MachineWord]),
    }

    fn test(test_name: &str, starting_memory: &[MachineWord], expected_pauses: &[ExpectedPause]) {
        let mut machine = starting_memory.iter().collect::<Machine>();

        for (step, expected_pause) in expected_pauses.iter().enumerate() {
            match expected_pause {
                In(input) => match machine.execute() {
                    Ok(AwaitingInput(new_machine)) => machine = new_machine.push_input(*input),
                    Ok(HasOutput(_, _)) => panic!(
                        "{}, step {}, is outputting instead of awaiting input",
                        test_name, step
                    ),
                    Ok(Halted(_)) => panic!(
                        "{}, step {}, is halting instead of awaiting input",
                        test_name, step
                    ),
                    Err(_) => panic!(
                        "{}, step {}, has an error instead of awaiting input",
                        test_name, step
                    ),
                },
                InWith(input, memory_after) => match machine.execute() {
                    Ok(AwaitingInput(new_machine)) => {
                        machine = new_machine.push_input(*input);
                        assert_eq!(*machine.memory(), memory_after.iter().collect(), "{}, step {}, has outputted the correct value, but has incorrect memory contents", test_name, step);
                    }
                    Ok(HasOutput(_, _)) => panic!(
                        "{}, step {}, is outputting instead of awaiting input",
                        test_name, step
                    ),
                    Ok(Halted(_)) => panic!(
                        "{}, step {}, is halting instead of awaiting input",
                        test_name, step
                    ),
                    Err(_) => panic!(
                        "{}, step {}, has an error instead of awaiting input",
                        test_name, step
                    ),
                },
                Out(expected_output) => match machine.execute() {
                    Ok(AwaitingInput(_)) => panic!(
                        "{}, step {}, is awaiting input instead of outputting",
                        test_name, step
                    ),
                    Ok(HasOutput(new_machine, output)) => {
                        assert_eq!(
                            output, *expected_output,
                            "{}, step {}, has given the wrong output",
                            test_name, step
                        );
                        machine = new_machine;
                    }
                    Ok(Halted(_)) => panic!(
                        "{}, step {}, is halting instead of outputting",
                        test_name, step
                    ),
                    Err(_) => panic!(
                        "{}, step {}, has an error instead of outputting",
                        test_name, step
                    ),
                },
                OutWith(expected_output, memory_after) => match machine.execute() {
                    Ok(AwaitingInput(_)) => panic!(
                        "{}, step {}, is awaiting input instead of outputting",
                        test_name, step
                    ),
                    Ok(HasOutput(new_machine, output)) => {
                        assert_eq!(
                            output, *expected_output,
                            "{}, step {}, has given the wrong output",
                            test_name, step
                        );
                        assert_eq!(*new_machine.memory(), memory_after.iter().collect(), "{}, step {}, has outputted the correct value, but has incorrect memory contents", test_name, step);
                        machine = new_machine;
                    }
                    Ok(Halted(_)) => panic!(
                        "{}, step {}, is halting instead of outputting",
                        test_name, step
                    ),
                    Err(_) => panic!(
                        "{}, step {}, has an error instead of outputting",
                        test_name, step
                    ),
                },
                Hlt => match machine.execute() {
                    Ok(AwaitingInput(_)) => panic!(
                        "{}, step {}, is awaiting input instead of halting",
                        test_name, step
                    ),
                    Ok(HasOutput(_, _)) => panic!(
                        "{}, step {}, is outputting instead of halting",
                        test_name, step
                    ),
                    Ok(Halted(_)) => return,
                    Err(_) => panic!(
                        "{}, step {}, has an error instead of halting",
                        test_name, step
                    ),
                },
                HltWith(memory_after) => match machine.execute() {
                    Ok(AwaitingInput(_)) => panic!(
                        "{}, step {}, is awaiting input instead of halting",
                        test_name, step
                    ),
                    Ok(HasOutput(_, _)) => panic!(
                        "{}, step {}, is outputting instead of halting",
                        test_name, step
                    ),
                    Ok(Halted(machine)) => {
                        assert_eq!(
                            *machine.memory(),
                            memory_after.iter().collect(),
                            "{}, step {}, has halted, but has incorrect memory contents",
                            test_name,
                            step
                        );
                        return;
                    }
                    Err(_) => panic!(
                        "{}, step {}, has an error instead of halting",
                        test_name, step
                    ),
                },
            }
        }
    }
}
