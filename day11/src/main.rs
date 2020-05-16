use intcode::machine::{Machine, MachinePause, MachineWord};
use std::{
    cmp::{max, min},
    collections::HashMap,
};

fn main() {
    let machine = Machine::from_file("11.txt").unwrap();

    println!("{}", paint_it(machine.clone(), Color::Black).keys().count());

    let painted_locations = paint_it(machine.clone(), Color::White);
    let mut min_x = 0;
    let mut max_x = 0;
    let mut min_y = 0;
    let mut max_y = 0;
    for (x, y) in painted_locations.keys() {
        min_x = min(*x, min_x);
        max_x = max(*x, max_x);
        min_y = min(*y, min_y);
        max_y = max(*y, max_y);
    }
    (min_y..=max_y).for_each(|y| {
        println!(
            "{}",
            (min_x..=max_x)
                .map(|x| painted_locations
                    .get(&(x, y))
                    .copied()
                    .unwrap_or_default()
                    .to_char())
                .collect::<String>()
        )
    })
}

fn paint_it(
    mut machine: Machine,
    starting_color: Color,
) -> HashMap<(MachineWord, MachineWord), Color> {
    let mut painted_locations: HashMap<(MachineWord, MachineWord), Color> = HashMap::new();
    painted_locations.insert((0, 0), starting_color);

    let mut x = 0;
    let mut y = 0;
    let mut direction: Direction = Default::default();

    loop {
        match machine.execute() {
            Ok(MachinePause::Halted(_)) => break,
            Ok(MachinePause::AwaitingInput(new_machine)) => {
                machine = new_machine.push_input(
                    painted_locations
                        .get(&(x, y))
                        .copied()
                        .unwrap_or_default()
                        .to_word(),
                );
            }
            _ => panic!("machine not awaiting input"),
        }
        match machine.execute() {
            Ok(MachinePause::HasOutput(new_machine, color)) => {
                painted_locations.insert((x, y), Color::from_word(&color));
                machine = new_machine;
            }
            _ => panic!("machine didn't provide new color"),
        }
        match machine.execute() {
            Ok(MachinePause::HasOutput(new_machine, turn)) => {
                direction = direction.turn_word(&turn);
                machine = new_machine;
            }
            _ => panic!("machine didn't provide a new turn"),
        }

        match direction {
            Direction::Up => y -= 1,
            Direction::Right => x += 1,
            Direction::Down => y += 1,
            Direction::Left => x -= 1,
        }
    }

    painted_locations
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Color {
    Black,
    White,
}

impl Color {
    pub fn from_word(word: &MachineWord) -> Color {
        match word {
            0 => Color::Black,
            1 => Color::White,
            _ => panic!("invalid color number"),
        }
    }

    pub fn to_word(&self) -> MachineWord {
        match self {
            Color::Black => 0,
            Color::White => 1,
        }
    }

    pub fn to_char(&self) -> char {
        match self {
            Color::Black => ' ',
            Color::White => '█',
        }
    }
}

impl Default for Color {
    fn default() -> Self {
        Color::Black
    }
}

impl Eq for Color {}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Direction {
    Up,
    Right,
    Down,
    Left,
}

impl Direction {
    pub fn turn_left(&self) -> Direction {
        match self {
            Direction::Up => Direction::Left,
            Direction::Right => Direction::Up,
            Direction::Down => Direction::Right,
            Direction::Left => Direction::Down,
        }
    }

    pub fn turn_right(&self) -> Direction {
        match self {
            Direction::Up => Direction::Right,
            Direction::Right => Direction::Down,
            Direction::Down => Direction::Left,
            Direction::Left => Direction::Up,
        }
    }

    pub fn turn_word(&self, word: &MachineWord) -> Direction {
        match word {
            0 => self.turn_left(),
            1 => self.turn_right(),
            _ => panic!("invalid turn number"),
        }
    }
}

impl Default for Direction {
    fn default() -> Self {
        Direction::Up
    }
}

impl Eq for Direction {}
