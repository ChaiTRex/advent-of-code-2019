use num::integer::lcm;
use std::{
    cmp::{max, Ordering},
    collections::{hash_map::Entry, HashMap},
    fs::File,
    io::{BufRead, BufReader},
    ops::{Add, AddAssign},
    str::FromStr,
};

fn main() {
    let moons = BufReader::new(File::open("../12.txt").unwrap())
        .lines()
        .map(|line| Moon::new(&line.unwrap().parse::<Position>().unwrap()))
        .collect::<Vec<_>>();

    {
        let mut moons = moons.clone();
        (0..1000).for_each(|_| apply_time_step(&mut moons));

        println!(
            "{}",
            moons.iter().map(|moon| moon.total_energy()).sum::<u32>()
        );
    }

    {
        let mut moons = moons.clone();

        let mut x_iterations = HashMap::new();
        let mut y_iterations = HashMap::new();
        let mut z_iterations = HashMap::new();

        let mut x_found = false;
        let mut x_start = 0;
        let mut x_cycle_length = 0;

        let mut y_found = false;
        let mut y_start = 0;
        let mut y_cycle_length = 0;

        let mut z_found = false;
        let mut z_start = 0;
        let mut z_cycle_length = 0;

        for iteration in 0u128.. {
            if x_found && y_found && z_found {
                break;
            }

            if !x_found {
                let xs = moons
                    .iter()
                    .map(|moon| (moon.position().x(), moon.velocity().x()))
                    .collect::<Vec<_>>();
                match x_iterations.entry(xs) {
                    Entry::Vacant(entry) => {
                        entry.insert(iteration);
                    }
                    Entry::Occupied(entry) => {
                        x_found = true;
                        x_start = *entry.get();
                        x_cycle_length = iteration - x_start;
                    }
                }
            }
            if !y_found {
                let ys = moons
                    .iter()
                    .map(|moon| (moon.position().y(), moon.velocity().y()))
                    .collect::<Vec<_>>();
                match y_iterations.entry(ys) {
                    Entry::Vacant(entry) => {
                        entry.insert(iteration);
                    }
                    Entry::Occupied(entry) => {
                        y_found = true;
                        y_start = *entry.get();
                        y_cycle_length = iteration - y_start;
                    }
                }
            }
            if !z_found {
                let zs = moons
                    .iter()
                    .map(|moon| (moon.position().z(), moon.velocity().z()))
                    .collect::<Vec<_>>();
                match z_iterations.entry(zs) {
                    Entry::Vacant(entry) => {
                        entry.insert(iteration);
                    }
                    Entry::Occupied(entry) => {
                        z_found = true;
                        z_start = *entry.get();
                        z_cycle_length = iteration - z_start;
                    }
                }
            }

            apply_time_step(&mut moons);
        }

        let start = max(max(x_start, y_start), z_start);
        let cycle_length = lcm(lcm(x_cycle_length, y_cycle_length), z_cycle_length);
        println!("{}", start + cycle_length);
    }
}

fn apply_time_step(moons: &mut [Moon]) {
    (1..moons.len()).for_each(|i| {
        let (left, right) = moons.split_at_mut(i);
        let left_moon = &mut left[i - 1];
        right.iter_mut().for_each(|mut right_moon| {
            left_moon.apply_gravity(&mut right_moon);
        });
    });
    moons.iter_mut().for_each(|moon| moon.apply_velocity());
}

#[derive(Clone, Copy, Debug, PartialEq)]
struct Moon {
    position: Position,
    velocity: Velocity,
}

impl Moon {
    fn new(position: &Position) -> Moon {
        Moon {
            position: *position,
            velocity: Default::default(),
        }
    }

    fn total_energy(&self) -> u32 {
        self.position.potential_energy() * self.velocity.kinetic_energy()
    }

    fn position(&self) -> &Position {
        &self.position
    }

    fn velocity(&self) -> &Velocity {
        &self.velocity
    }

    #[inline(always)]
    fn apply_gravity(&mut self, other: &mut Self) {
        match self.position.x.cmp(&other.position.x) {
            Ordering::Less => {
                self.velocity.x += 1;
                other.velocity.x -= 1;
            }
            Ordering::Equal => (),
            Ordering::Greater => {
                self.velocity.x -= 1;
                other.velocity.x += 1;
            }
        }
        match self.position.y.cmp(&other.position.y) {
            Ordering::Less => {
                self.velocity.y += 1;
                other.velocity.y -= 1;
            }
            Ordering::Equal => (),
            Ordering::Greater => {
                self.velocity.y -= 1;
                other.velocity.y += 1;
            }
        }
        match self.position.z.cmp(&other.position.z) {
            Ordering::Less => {
                self.velocity.z += 1;
                other.velocity.z -= 1;
            }
            Ordering::Equal => (),
            Ordering::Greater => {
                self.velocity.z -= 1;
                other.velocity.z += 1;
            }
        }
    }

    #[inline(always)]
    fn apply_velocity(&mut self) {
        self.position += self.velocity;
    }
}

impl Eq for Moon {}

#[derive(Clone, Copy, Debug, PartialEq)]
struct Position {
    x: i32,
    y: i32,
    z: i32,
}

impl Position {
    fn x(&self) -> i32 {
        self.x
    }

    fn y(&self) -> i32 {
        self.y
    }

    fn z(&self) -> i32 {
        self.z
    }

    fn potential_energy(&self) -> u32 {
        self.x.abs() as u32 + self.y.abs() as u32 + self.z.abs() as u32
    }
}

impl Add<Velocity> for Position {
    type Output = Position;

    fn add(self, rhs: Velocity) -> Self::Output {
        Position {
            x: self.x + rhs.x,
            y: self.y + rhs.y,
            z: self.z + rhs.z,
        }
    }
}

impl Add<&Velocity> for Position {
    type Output = Position;

    fn add(self, rhs: &Velocity) -> Self::Output {
        Position {
            x: self.x + rhs.x,
            y: self.y + rhs.y,
            z: self.z + rhs.z,
        }
    }
}

impl AddAssign<Velocity> for Position {
    fn add_assign(&mut self, rhs: Velocity) {
        self.x += rhs.x;
        self.y += rhs.y;
        self.z += rhs.z;
    }
}

impl AddAssign<&Velocity> for Position {
    fn add_assign(&mut self, rhs: &Velocity) {
        self.x += rhs.x;
        self.y += rhs.y;
        self.z += rhs.z;
    }
}

impl Eq for Position {}

impl FromStr for Position {
    type Err = &'static str;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        fn parse_variable(
            chars: &mut impl Iterator<Item = char>,
            variable_letter: char,
        ) -> Result<(Option<char>, i32), &'static str> {
            if chars.next() != Some(variable_letter) {
                return Err("missing correct variable");
            }
            if chars.next() != Some('=') {
                return Err("missing '='");
            }

            let mut ch = chars.next();
            let negative = if ch == Some('-') {
                ch = chars.next();
                true
            } else {
                false
            };
            let mut value = 0;
            loop {
                match ch {
                    Some(d @ '0'..='9') => value = 10 * value + (d as i32 - '0' as i32),
                    Some(_) => break,
                    _ => return Err("unfinished value"),
                }
                ch = chars.next();
            }
            if negative {
                value = -value;
            }
            Ok((ch, value))
        }

        let mut chars = s.chars();

        if chars.next() != Some('<') {
            return Err("missing starting '<'");
        }

        let (ch, x) = parse_variable(&mut chars, 'x')?;

        if ch != Some(',') {
            return Err("missing ','");
        }
        if chars.next() != Some(' ') {
            return Err("missing ' '");
        }

        let (ch, y) = parse_variable(&mut chars, 'y')?;

        if ch != Some(',') {
            return Err("missing ','");
        }
        if chars.next() != Some(' ') {
            return Err("missing ' '");
        }

        let (ch, z) = parse_variable(&mut chars, 'z')?;

        if ch != Some('>') {
            return Err("missing ending '>'");
        }
        if chars.next() != None {
            return Err("extra characters after '>'");
        }

        Ok(Position { x, y, z })
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
struct Velocity {
    x: i32,
    y: i32,
    z: i32,
}

impl Velocity {
    fn x(&self) -> i32 {
        self.x
    }

    fn y(&self) -> i32 {
        self.y
    }

    fn z(&self) -> i32 {
        self.z
    }

    fn kinetic_energy(&self) -> u32 {
        self.x.abs() as u32 + self.y.abs() as u32 + self.z.abs() as u32
    }
}

impl Default for Velocity {
    fn default() -> Self {
        Velocity { x: 0, y: 0, z: 0 }
    }
}

impl Eq for Velocity {}
