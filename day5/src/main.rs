use intcode::machine::{Machine, MachinePause, MachineWord};

fn main() {
    let machine = Machine::from_file("5.txt").unwrap();
    println!("{}", get_output(machine.clone(), 1));
    println!("{}", get_output(machine.clone(), 5));
}

fn get_output(mut machine: Machine, input: MachineWord) -> MachineWord {
    match machine.execute().unwrap() {
        MachinePause::AwaitingInput(new_machine) => {
            machine = new_machine.push_input(input);
        }
        _ => panic!("machine was not awaiting input to start with"),
    }

    let output = loop {
        match machine.execute().unwrap() {
            MachinePause::HasOutput(new_machine, the_output) => {
                machine = new_machine;
                if the_output != 0 {
                    break the_output;
                }
            }
            _ => panic!("machine did not produce output after input"),
        }
    };

    match machine.execute().unwrap() {
        MachinePause::Halted(_) => return output,
        _ => panic!("machine did not halt after output"),
    }
}
