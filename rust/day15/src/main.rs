use intcode::machine::{Machine, MachinePause, MachineWord};
use std::{collections::HashSet, hash::Hash};

fn main() {
    let mut main_machine = Machine::from_file("../15.txt").unwrap();

    {
        let mut visited_positions = HashSet::new();
        let mut machines = vec![(CENTER.clone(), main_machine.clone())];

        let mut movements = 0;
        'outer: loop {
            movements += 1;
            let mut new_machines = Vec::with_capacity(machines.len());
            for (position, machine) in machines.into_iter() {
                for direction in DIRECTIONS.iter() {
                    let new_position = position.travel(&direction);
                    if !visited_positions.contains(&new_position) {
                        let mut machine = machine.clone();
                        match machine.execute() {
                            Ok(MachinePause::AwaitingInput(new_machine)) => {
                                machine = new_machine.push_input(direction.to_word());
                            }
                            _ => panic!("machine not awaiting input"),
                        }
                        match machine.execute() {
                            Ok(MachinePause::HasOutput(new_machine, output)) => {
                                match Status::from_word(&output) {
                                    Status::HitAWall => (),
                                    Status::Ok => {
                                        visited_positions.insert(new_position.clone());
                                        new_machines.push((new_position, new_machine));
                                    }
                                    Status::FoundOxygenSystem => {
                                        main_machine = new_machine;
                                        break 'outer;
                                    }
                                }
                            }
                            _ => panic!("machine did not produce output"),
                        }
                    }
                }
            }
            machines = new_machines;
        }

        println!("{}", movements);
    }

    {
        let mut visited_positions = HashSet::new();
        visited_positions.insert(CENTER.clone());
        let mut machines = vec![(CENTER.clone(), main_machine.clone())];

        let mut movements = 0;
        while machines.len() != 0 {
            movements += 1;
            let mut new_machines = Vec::with_capacity(machines.len());
            for (position, machine) in machines.into_iter() {
                for direction in DIRECTIONS.iter() {
                    let new_position = position.travel(&direction);
                    if !visited_positions.contains(&new_position) {
                        let mut machine = machine.clone();
                        match machine.execute() {
                            Ok(MachinePause::AwaitingInput(new_machine)) => {
                                machine = new_machine.push_input(direction.to_word());
                            }
                            _ => panic!("machine not awaiting input"),
                        }
                        match machine.execute() {
                            Ok(MachinePause::HasOutput(new_machine, output)) => {
                                match Status::from_word(&output) {
                                    Status::HitAWall => (),
                                    _ => {
                                        visited_positions.insert(new_position.clone());
                                        new_machines.push((new_position, new_machine));
                                    }
                                }
                            }
                            _ => panic!("machine did not produce output"),
                        }
                    }
                }
            }
            machines = new_machines;
        }

        println!("{}", movements - 1);
    }
}

#[derive(Clone, Debug, Hash, PartialEq)]
struct Position {
    x: MachineWord,
    y: MachineWord,
}

const CENTER: Position = Position { x: 0, y: 0 };

impl Position {
    fn travel(&self, direction: &Direction) -> Position {
        let x = self.x.clone();
        let y = self.y.clone();

        match direction {
            Direction::North => Position { x, y: y - 1 },
            Direction::East => Position { x: x + 1, y },
            Direction::South => Position { x, y: y + 1 },
            Direction::West => Position { x: x - 1, y },
        }
    }
}

impl Eq for Position {}

enum Direction {
    North,
    East,
    South,
    West,
}

const DIRECTIONS: [Direction; 4] = [
    Direction::North,
    Direction::East,
    Direction::South,
    Direction::West,
];

impl Direction {
    fn to_word(&self) -> MachineWord {
        match self {
            Direction::North => 1,
            Direction::East => 4,
            Direction::South => 2,
            Direction::West => 3,
        }
    }
}

enum Status {
    HitAWall,
    Ok,
    FoundOxygenSystem,
}

impl Status {
    fn from_word(word: &MachineWord) -> Status {
        match word {
            0 => Status::HitAWall,
            1 => Status::Ok,
            2 => Status::FoundOxygenSystem,
            _ => panic!("invalid status word"),
        }
    }
}
