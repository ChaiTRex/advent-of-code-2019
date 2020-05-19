use intcode::machine::{Machine, MachinePause, MachineWord};
use itertools::Itertools;

fn main() {
    let machine = Machine::from_file("../7.txt").unwrap();

    let maximum_signal = (0..=4)
        .permutations(5)
        .map(|phases| {
            let mut machines = [None, None, None, None, None];
            for i in 0..5 {
                machines[i] = Some(machine.clone());
                push_input(&mut machines[i], phases[i]);
            }
            push_input(&mut machines[0], 0);
            for i in 1..5 {
                let output = get_output(&mut machines[i - 1]);
                push_input(&mut machines[i], output);
            }
            get_output(&mut machines[4])
        })
        .max()
        .unwrap();
    println!("{}", maximum_signal);

    let maximum_signal = (5..=9)
        .permutations(5)
        .map(|phases| {
            let mut machines = [None, None, None, None, None];
            for i in 0..5 {
                machines[i] = Some(machine.clone());
                push_input(&mut machines[i], phases[i]);
            }
            push_input(&mut machines[0], 0);

            'outer: loop {
                for i in 1..6 {
                    let output = get_output(&mut machines[i - 1]);
                    if !push_input(&mut machines[i % 5], output) {
                        break 'outer output;
                    }
                }
            }
        })
        .max()
        .unwrap();
    println!("{}", maximum_signal);
}

fn push_input(machine: &mut Option<Machine>, value: MachineWord) -> bool {
    match machine.take().unwrap().execute() {
        Ok(MachinePause::AwaitingInput(new_machine)) => {
            machine.replace(new_machine.push_input(value));
            true
        }
        Ok(MachinePause::Halted(_)) => false,
        _ => panic!("machine wasn't awaiting input"),
    }
}

fn get_output(machine: &mut Option<Machine>) -> MachineWord {
    match machine.take().unwrap().execute() {
        Ok(MachinePause::HasOutput(new_machine, output)) => {
            machine.replace(new_machine);
            output
        }
        _ => panic!("machine wasn't producing output"),
    }
}
