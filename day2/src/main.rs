use intcode::machine::{ Machine, MachinePause };

fn main() {
    let original_machine = Machine::from_file("2.txt").unwrap();
    
    let mut machine = original_machine.clone();
    let memory = machine.memory_mut();
    memory[1] = 12;
    memory[2] = 2;
    match machine.execute() {
        Ok(MachinePause::Halted(machine)) => println!("{}", machine.memory()[0]),
        _ => panic!("Machine didn't halt"),
    }

    'outer_loop:
    for noun in 0 ..= 99 {
        for verb in 0 ..= 99 {
            let mut machine = original_machine.clone();
            let memory = machine.memory_mut();
            memory[1] = noun;
            memory[2] = verb;
            match machine.execute() {
                Ok(MachinePause::Halted(machine)) => {
                    if machine.memory()[0] == 19690720 {
                        println!("{}", 100*noun + verb);
                        break 'outer_loop;
                    }
                },
                _ => (),
            }
        }
    }
}
