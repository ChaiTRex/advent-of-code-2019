use day10::Vector;
use itertools::Itertools;
use std::{
    fs::File,
    io::{BufRead, BufReader},
};

fn main() {
    let asteroids = (0i32..)
        .zip(
            BufReader::new(File::open("../10.txt").unwrap())
                .lines()
                .map(Result::unwrap),
        )
        .map(|(y, line)| {
            (0i32..)
                .zip(line.chars())
                .filter(|(_, ch)| *ch == '#')
                .map(|(x, _)| (x, y))
                .collect::<Vec<_>>()
        })
        .flatten()
        .collect::<Vec<_>>();

    let (visible_asteroid_count, vectors_by_angle) = asteroids
        .iter()
        .map(|(x, y)| {
            let mut vectors_from_asteroid = asteroids
                .iter()
                .filter(|(x2, y2)| x != x2 || y != y2)
                .map(|(x2, y2)| (Vector::new(x2 - x, y - y2), x2, y2))
                .collect::<Vec<_>>();
            vectors_from_asteroid.sort();
            let grouped_vectors = vectors_from_asteroid
                .into_iter()
                .group_by(|(v, _, _)| v.angle())
                .into_iter()
                .map(|(_, group)| group.collect::<Vec<_>>())
                .collect::<Vec<_>>();
            (grouped_vectors.len(), grouped_vectors)
        })
        .max()
        .unwrap();

    println!("{}", visible_asteroid_count);

    match (0..vectors_by_angle.iter().map(Vec::len).max().unwrap())
        .map(|i| {
            vectors_by_angle
                .iter()
                .map(|vs| if i < vs.len() { Some(vs[i]) } else { None })
                .flatten()
                .collect::<Vec<_>>()
        })
        .flatten()
        .nth(199)
    {
        Some((_, x, y)) => println!("{}", 100 * x + y),
        None => panic!("not enough asteroids"),
    }
}
