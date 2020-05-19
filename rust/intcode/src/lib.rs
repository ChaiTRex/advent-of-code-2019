pub mod machine;
pub mod memory;

#[cfg(test)]
mod tests {
    use self::ExpectedPause::*;
    use crate::machine::{
        Machine,
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
            "day 7, example 0, machine 0",
            &[
                3, 15, 3, 16, 1002, 16, 10, 16, 1, 16, 15, 15, 4, 15, 99, 0, 0,
            ],
            &[In(4), In(0), Out(4), Hlt],
        );
        test(
            "day 7, example 0, machine 1",
            &[
                3, 15, 3, 16, 1002, 16, 10, 16, 1, 16, 15, 15, 4, 15, 99, 0, 0,
            ],
            &[In(3), In(4), Out(43), Hlt],
        );
        test(
            "day 7, example 0, machine 2",
            &[
                3, 15, 3, 16, 1002, 16, 10, 16, 1, 16, 15, 15, 4, 15, 99, 0, 0,
            ],
            &[In(2), In(43), Out(432), Hlt],
        );
        test(
            "day 7, example 0, machine 3",
            &[
                3, 15, 3, 16, 1002, 16, 10, 16, 1, 16, 15, 15, 4, 15, 99, 0, 0,
            ],
            &[In(1), In(432), Out(4321), Hlt],
        );
        test(
            "day 7, example 0, machine 4",
            &[
                3, 15, 3, 16, 1002, 16, 10, 16, 1, 16, 15, 15, 4, 15, 99, 0, 0,
            ],
            &[In(0), In(4321), Out(43210), Hlt],
        );
        test(
            "day 7, example 1, machine 0",
            &[
                3, 23, 3, 24, 1002, 24, 10, 24, 1002, 23, -1, 23, 101, 5, 23, 23, 1, 24, 23, 23, 4,
                23, 99, 0, 0,
            ],
            &[In(0), In(0), Out(5), Hlt],
        );
        test(
            "day 7, example 1, machine 1",
            &[
                3, 23, 3, 24, 1002, 24, 10, 24, 1002, 23, -1, 23, 101, 5, 23, 23, 1, 24, 23, 23, 4,
                23, 99, 0, 0,
            ],
            &[In(1), In(5), Out(54), Hlt],
        );
        test(
            "day 7, example 1, machine 2",
            &[
                3, 23, 3, 24, 1002, 24, 10, 24, 1002, 23, -1, 23, 101, 5, 23, 23, 1, 24, 23, 23, 4,
                23, 99, 0, 0,
            ],
            &[In(2), In(54), Out(543), Hlt],
        );
        test(
            "day 7, example 1, machine 3",
            &[
                3, 23, 3, 24, 1002, 24, 10, 24, 1002, 23, -1, 23, 101, 5, 23, 23, 1, 24, 23, 23, 4,
                23, 99, 0, 0,
            ],
            &[In(3), In(543), Out(5432), Hlt],
        );
        test(
            "day 7, example 1, machine 4",
            &[
                3, 23, 3, 24, 1002, 24, 10, 24, 1002, 23, -1, 23, 101, 5, 23, 23, 1, 24, 23, 23, 4,
                23, 99, 0, 0,
            ],
            &[In(4), In(5432), Out(54321), Hlt],
        );
        test(
            "day 7, example 2, machine 0",
            &[
                3, 31, 3, 32, 1002, 32, 10, 32, 1001, 31, -2, 31, 1007, 31, 0, 33, 1002, 33, 7, 33,
                1, 33, 31, 31, 1, 32, 31, 31, 4, 31, 99, 0, 0, 0,
            ],
            &[In(1), In(0), Out(6)],
        );
        test(
            "day 7, example 2, machine 1",
            &[
                3, 31, 3, 32, 1002, 32, 10, 32, 1001, 31, -2, 31, 1007, 31, 0, 33, 1002, 33, 7, 33,
                1, 33, 31, 31, 1, 32, 31, 31, 4, 31, 99, 0, 0, 0,
            ],
            &[In(0), In(6), Out(65)],
        );
        test(
            "day 7, example 2, machine 2",
            &[
                3, 31, 3, 32, 1002, 32, 10, 32, 1001, 31, -2, 31, 1007, 31, 0, 33, 1002, 33, 7, 33,
                1, 33, 31, 31, 1, 32, 31, 31, 4, 31, 99, 0, 0, 0,
            ],
            &[In(4), In(65), Out(652)],
        );
        test(
            "day 7, example 2, machine 3",
            &[
                3, 31, 3, 32, 1002, 32, 10, 32, 1001, 31, -2, 31, 1007, 31, 0, 33, 1002, 33, 7, 33,
                1, 33, 31, 31, 1, 32, 31, 31, 4, 31, 99, 0, 0, 0,
            ],
            &[In(3), In(652), Out(6521)],
        );
        test(
            "day 7, example 2, machine 4",
            &[
                3, 31, 3, 32, 1002, 32, 10, 32, 1001, 31, -2, 31, 1007, 31, 0, 33, 1002, 33, 7, 33,
                1, 33, 31, 31, 1, 32, 31, 31, 4, 31, 99, 0, 0, 0,
            ],
            &[In(2), In(6521), Out(65210)],
        );
        test(
            "day 7, example 3, machine 0",
            &[
                3, 26, 1001, 26, -4, 26, 3, 27, 1002, 27, 2, 27, 1, 27, 26, 27, 4, 27, 1001, 28,
                -1, 28, 1005, 28, 6, 99, 0, 0, 5,
            ],
            &[
                In(9),
                In(0),
                Out(5),
                In(129),
                Out(263),
                In(4257),
                Out(8519),
                In(136353),
                Out(272711),
                In(4363425),
                Out(8726855),
                Hlt,
            ],
        );
        test(
            "day 7, example 3, machine 1",
            &[
                3, 26, 1001, 26, -4, 26, 3, 27, 1002, 27, 2, 27, 1, 27, 26, 27, 4, 27, 1001, 28,
                -1, 28, 1005, 28, 6, 99, 0, 0, 5,
            ],
            &[
                In(8),
                In(5),
                Out(14),
                In(263),
                Out(530),
                In(8519),
                Out(17042),
                In(272711),
                Out(545426),
                In(8726855),
                Out(17453714),
                Hlt,
            ],
        );
        test(
            "day 7, example 3, machine 2",
            &[
                3, 26, 1001, 26, -4, 26, 3, 27, 1002, 27, 2, 27, 1, 27, 26, 27, 4, 27, 1001, 28,
                -1, 28, 1005, 28, 6, 99, 0, 0, 5,
            ],
            &[
                In(7),
                In(14),
                Out(31),
                In(530),
                Out(1063),
                In(17042),
                Out(34087),
                In(545426),
                Out(1090855),
                In(17453714),
                Out(34907431),
                Hlt,
            ],
        );
        test(
            "day 7, example 3, machine 3",
            &[
                3, 26, 1001, 26, -4, 26, 3, 27, 1002, 27, 2, 27, 1, 27, 26, 27, 4, 27, 1001, 28,
                -1, 28, 1005, 28, 6, 99, 0, 0, 5,
            ],
            &[
                In(6),
                In(31),
                Out(64),
                In(1063),
                Out(2128),
                In(34087),
                Out(68176),
                In(1090855),
                Out(2181712),
                In(34907431),
                Out(69814864),
                Hlt,
            ],
        );
        test(
            "day 7, example 3, machine 4",
            &[
                3, 26, 1001, 26, -4, 26, 3, 27, 1002, 27, 2, 27, 1, 27, 26, 27, 4, 27, 1001, 28,
                -1, 28, 1005, 28, 6, 99, 0, 0, 5,
            ],
            &[
                In(5),
                In(64),
                Out(129),
                In(2128),
                Out(4257),
                In(68176),
                Out(136353),
                In(2181712),
                Out(4363425),
                In(69814864),
                Out(139629729),
                Hlt,
            ],
        );
        test(
            "day 7, example 4, machine 0",
            &[
                3, 52, 1001, 52, -5, 52, 3, 53, 1, 52, 56, 54, 1007, 54, 5, 55, 1005, 55, 26, 1001,
                54, -5, 54, 1105, 1, 12, 1, 53, 54, 53, 1008, 54, 0, 55, 1001, 55, 1, 55, 2, 53,
                55, 53, 4, 53, 1001, 56, -1, 56, 1005, 56, 6, 99, 0, 0, 0, 0, 10,
            ],
            &[
                In(9),
                In(0),
                Out(4),
                In(19),
                Out(22),
                In(58),
                Out(60),
                In(128),
                Out(129),
                In(271),
                Out(542),
                In(552),
                Out(556),
                In(1123),
                Out(1126),
                In(2266),
                Out(2268),
                In(4544),
                Out(4545),
                In(9103),
                Out(18206),
                Hlt,
            ],
        );
        test(
            "day 7, example 4, machine 1",
            &[
                3, 52, 1001, 52, -5, 52, 3, 53, 1, 52, 56, 54, 1007, 54, 5, 55, 1005, 55, 26, 1001,
                54, -5, 54, 1105, 1, 12, 1, 53, 54, 53, 1008, 54, 0, 55, 1001, 55, 1, 55, 2, 53,
                55, 53, 4, 53, 1001, 56, -1, 56, 1005, 56, 6, 99, 0, 0, 0, 0, 10,
            ],
            &[
                In(7),
                In(4),
                Out(6),
                In(22),
                Out(23),
                In(60),
                Out(120),
                In(129),
                Out(133),
                In(542),
                Out(545),
                In(556),
                Out(558),
                In(1126),
                Out(1127),
                In(2268),
                Out(4536),
                In(4545),
                Out(4549),
                In(18206),
                Out(18209),
                Hlt,
            ],
        );
        test(
            "day 7, example 4, machine 2",
            &[
                3, 52, 1001, 52, -5, 52, 3, 53, 1, 52, 56, 54, 1007, 54, 5, 55, 1005, 55, 26, 1001,
                54, -5, 54, 1105, 1, 12, 1, 53, 54, 53, 1008, 54, 0, 55, 1001, 55, 1, 55, 2, 53,
                55, 53, 4, 53, 1001, 56, -1, 56, 1005, 56, 6, 99, 0, 0, 0, 0, 10,
            ],
            &[
                In(8),
                In(6),
                Out(9),
                In(23),
                Out(25),
                In(120),
                Out(121),
                In(133),
                Out(266),
                In(545),
                Out(549),
                In(558),
                Out(561),
                In(1127),
                Out(1129),
                In(4536),
                Out(4537),
                In(4549),
                Out(9098),
                In(18209),
                Out(18213),
                Hlt,
            ],
        );
        test(
            "day 7, example 4, machine 3",
            &[
                3, 52, 1001, 52, -5, 52, 3, 53, 1, 52, 56, 54, 1007, 54, 5, 55, 1005, 55, 26, 1001,
                54, -5, 54, 1105, 1, 12, 1, 53, 54, 53, 1008, 54, 0, 55, 1001, 55, 1, 55, 2, 53,
                55, 53, 4, 53, 1001, 56, -1, 56, 1005, 56, 6, 99, 0, 0, 0, 0, 10,
            ],
            &[
                In(5),
                In(9),
                Out(18),
                In(25),
                Out(29),
                In(121),
                Out(124),
                In(266),
                Out(268),
                In(549),
                Out(550),
                In(561),
                Out(1122),
                In(1129),
                Out(1133),
                In(4537),
                Out(4540),
                In(9098),
                Out(9100),
                In(18213),
                Out(18214),
                Hlt,
            ],
        );
        test(
            "day 7, example 4, machine 4",
            &[
                3, 52, 1001, 52, -5, 52, 3, 53, 1, 52, 56, 54, 1007, 54, 5, 55, 1005, 55, 26, 1001,
                54, -5, 54, 1105, 1, 12, 1, 53, 54, 53, 1008, 54, 0, 55, 1001, 55, 1, 55, 2, 53,
                55, 53, 4, 53, 1001, 56, -1, 56, 1005, 56, 6, 99, 0, 0, 0, 0, 10,
            ],
            &[
                In(6),
                In(18),
                Out(19),
                In(29),
                Out(58),
                In(124),
                Out(128),
                In(268),
                Out(271),
                In(550),
                Out(552),
                In(1122),
                Out(1123),
                In(1133),
                Out(2266),
                In(4540),
                Out(4544),
                In(9100),
                Out(9103),
                In(18214),
                Out(18216),
                Hlt,
            ],
        );
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
