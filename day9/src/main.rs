use intcode::machine::{Machine, MachinePause, MachineWord};

fn main() {
    let machine = Machine::from_file("9.txt").unwrap();

    println!("{}", single_input_single_output(machine.clone(), 1));
    println!("{}", single_input_single_output(machine.clone(), 2));
}

fn single_input_single_output(mut machine: Machine, input: MachineWord) -> MachineWord {
    match machine.execute() {
        Ok(MachinePause::AwaitingInput(new_machine)) => {
            machine = new_machine.push_input(input);
        }
        _ => panic!("machine not awaiting input"),
    }
    let result;
    match machine.execute() {
        Ok(MachinePause::HasOutput(new_machine, output)) => {
            machine = new_machine;
            result = output;
        }
        _ => panic!("machine not producing output"),
    }
    match machine.execute() {
        Ok(MachinePause::Halted(_)) => result,
        _ => panic!("machine didn't halt after first output"),
    }
}
