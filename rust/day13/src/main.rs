use intcode::machine::{Machine, MachinePause, MachineWord};
use std::{
    cmp::{max, min, Ordering},
    collections::HashMap,
};

fn main() {
    let machine = Machine::from_file("../13.txt").unwrap();

    {
        let mut machine = machine.clone();
        let mut board = HashMap::new();
        loop {
            let x = match machine.execute() {
                Ok(MachinePause::Halted(_)) => break,
                Ok(MachinePause::HasOutput(new_machine, x)) => {
                    machine = new_machine;
                    x
                }
                _ => panic!("machine did not output x value or halt"),
            };

            let y = match machine.execute() {
                Ok(MachinePause::HasOutput(new_machine, y)) => {
                    machine = new_machine;
                    y
                }
                _ => panic!("machine did not output y"),
            };

            let tile = match machine.execute() {
                Ok(MachinePause::HasOutput(new_machine, tile)) => {
                    machine = new_machine;
                    Tile::from_word(&tile)
                }
                _ => panic!("machine did not output tile type"),
            };
            board.insert((x, y), tile);
        }

        println!(
            "{}",
            board.values().filter(|tile| **tile == Tile::Block).count()
        );
    }

    {
        let mut machine = machine.clone();
        machine.memory_mut()[0] = 2;

        let mut score = 0;
        let mut board = HashMap::new();

        let mut paddle_x = 0;
        let mut ball_x = 0;

        loop {
            match machine.execute() {
                Ok(MachinePause::Halted(_)) => break,
                Ok(MachinePause::AwaitingInput(new_machine)) => {
                    machine = new_machine.push_input(match ball_x.cmp(&paddle_x) {
                        Ordering::Less => -1,
                        Ordering::Equal => 0,
                        Ordering::Greater => 1,
                    });
                }
                Ok(MachinePause::HasOutput(new_machine, x)) => {
                    machine = new_machine;

                    let y = match machine.execute() {
                        Ok(MachinePause::HasOutput(new_machine, y)) => {
                            machine = new_machine;
                            y
                        }
                        _ => panic!("machine did not output y"),
                    };

                    let value = match machine.execute() {
                        Ok(MachinePause::HasOutput(new_machine, value)) => {
                            machine = new_machine;
                            value
                        }
                        _ => panic!("machine did not output tile type"),
                    };

                    if x == -1 && y == 0 {
                        score = value;
                    } else {
                        let tile = Tile::from_word(&value);
                        match tile {
                            Tile::Paddle => paddle_x = x,
                            Tile::Ball => ball_x = x,
                            _ => (),
                        }
                        board.insert((x, y), tile);
                    }
                }
                _ => panic!("machine did not output x value or halt"),
            };
        }

        println!("{}", score);
    }
}

#[allow(dead_code)]
fn display_game(board: &HashMap<(MachineWord, MachineWord), Tile>, score: MachineWord) {
    let mut min_x = MachineWord::MAX;
    let mut max_x = MachineWord::MIN;
    let mut min_y = MachineWord::MAX;
    let mut max_y = MachineWord::MIN;

    board.keys().for_each(|(x, y)| {
        min_x = min(min_x, *x);
        max_x = max(max_x, *x);
        min_y = min(min_y, *y);
        max_y = max(max_y, *y);
    });

    println!("Score: {}", score);
    (min_y..=max_y).for_each(|y| {
        println!(
            "{}",
            (min_x..=max_x)
                .map(|x| { board.get(&(x, y)).copied().unwrap_or_default().to_char() })
                .collect::<String>()
        );
    });
}

#[derive(Clone, Copy, Debug, PartialEq)]
enum Tile {
    Empty,
    Wall,
    Block,
    Paddle,
    Ball,
}

impl Tile {
    pub fn to_char(&self) -> char {
        match self {
            Tile::Empty => ' ',
            Tile::Wall => '█',
            Tile::Block => '▒',
            Tile::Paddle => '━',
            Tile::Ball => '◯',
        }
    }

    pub fn from_word(word: &MachineWord) -> Tile {
        match word {
            0 => Tile::Empty,
            1 => Tile::Wall,
            2 => Tile::Block,
            3 => Tile::Paddle,
            4 => Tile::Ball,
            _ => panic!("invalid tile type number"),
        }
    }
}

impl Default for Tile {
    fn default() -> Self {
        Tile::Empty
    }
}

impl Eq for Tile {}
