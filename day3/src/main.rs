use std::{
    collections::{HashMap, HashSet},
    error::Error,
    fmt::{self, Display, Formatter},
    fs::File,
    io::{BufRead, BufReader},
    iter::repeat,
    num::ParseIntError,
    ops::Add,
    str::FromStr,
};

fn main() {
    let wire1_points;
    let wire2_points;
    {
        let mut wires = BufReader::new(File::open("3.txt").unwrap())
            .lines()
            .map(|line| {
                line.unwrap()
                    .split(',')
                    .map(|vector| match vector.parse::<CardinalVector>().unwrap() {
                        CardinalVector::Up(n) => repeat(Point(0, 1)).take(n),
                        CardinalVector::Right(n) => repeat(Point(1, 0)).take(n),
                        CardinalVector::Down(n) => repeat(Point(0, -1)).take(n),
                        CardinalVector::Left(n) => repeat(Point(-1, 0)).take(n),
                    })
                    .flatten()
                    .scan(Point(0, 0), |state, step| {
                        *state = *state + step;
                        Some(*state)
                    })
                    .collect::<Vec<_>>()
            })
            .collect::<Vec<_>>();

        wire2_points = wires.pop().unwrap();
        wire1_points = wires.pop().unwrap();
    }

    let wire1_distances;
    {
        let mut map = HashMap::new();
        wire1_points.iter().enumerate().for_each(|(i, pt)| {
            map.entry(pt.clone()).or_insert(i);
        });
        wire1_distances = map;
    }

    let wire2_distances;
    {
        let mut map = HashMap::new();
        wire2_points.iter().enumerate().for_each(|(i, pt)| {
            map.entry(pt.clone()).or_insert(i);
        });
        wire2_distances = map;
    }

    let common_points;
    {
        let wire1_set = wire1_points.iter().collect::<HashSet<_>>();
        let wire2_set = wire2_points.iter().collect::<HashSet<_>>();
        common_points = wire1_set
            .intersection(&wire2_set)
            .cloned()
            .collect::<Vec<_>>();
    }

    println!(
        "{}",
        common_points
            .iter()
            .map(|pt| pt.0.abs() + pt.1.abs())
            .min()
            .unwrap(),
    );

    println!(
        "{}",
        common_points
            .iter()
            .map(|pt| wire1_distances.get(pt).unwrap() + wire2_distances.get(pt).unwrap() + 2)
            .min()
            .unwrap()
    );
}

#[derive(Clone, Copy, Debug, Hash, PartialEq)]
struct Point(i64, i64);

impl Add for Point {
    type Output = Self;

    fn add(self, other: Self) -> Self::Output {
        Point(self.0 + other.0, self.1 + other.1)
    }
}

impl Eq for Point {}

#[derive(Clone, Copy, Debug, PartialEq)]
enum CardinalVector {
    Up(usize),
    Right(usize),
    Down(usize),
    Left(usize),
}

impl Eq for CardinalVector {}

impl FromStr for CardinalVector {
    type Err = ParseCardinalVectorError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s.len() < 2 {
            return Err(ParseCardinalVectorError {
                message: "input too short",
                source: None,
            });
        }

        let f = match s.chars().next().unwrap() {
            'U' => CardinalVector::Up,
            'R' => CardinalVector::Right,
            'D' => CardinalVector::Down,
            'L' => CardinalVector::Left,
            _ => {
                return Err(ParseCardinalVectorError {
                    message: "first character not U, R, D, or L",
                    source: None,
                })
            }
        };

        let s = &s[1..];
        match s.parse() {
            Ok(x) => Ok(f(x)),
            Err(e) => Err(ParseCardinalVectorError {
                message: "number parsing error",
                source: Some(e),
            }),
        }
    }
}

#[derive(Debug)]
struct ParseCardinalVectorError {
    message: &'static str,
    source: Option<ParseIntError>,
}

impl Display for ParseCardinalVectorError {
    fn fmt(&self, mut f: &mut Formatter<'_>) -> fmt::Result {
        self.message.fmt(&mut f)
    }
}

impl Error for ParseCardinalVectorError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        self.source.as_ref().map(|x| x as &dyn Error)
    }
}
