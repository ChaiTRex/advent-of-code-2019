use itertools::Itertools;
use std::{
    fs::File,
    io::{BufRead, BufReader},
};

fn main() {
    BufReader::new(File::open("8.txt").unwrap())
        .lines()
        .for_each(|line| {
            let picture = line
                .unwrap()
                .chars()
                .chunks(25)
                .into_iter()
                .chunks(6)
                .into_iter()
                .map(|layer| {
                    layer
                        .map(|row| row.map(Color::parse).collect::<Vec<_>>())
                        .collect::<Vec<_>>()
                })
                .collect::<Vec<_>>();

            let (_, white, transparent) =
                picture
                    .iter()
                    .map(|layer| {
                        layer.iter().flatten().fold(
                            (0, 0, 0),
                            |(black, white, transparent), color| match color {
                                Color::Black => (black + 1, white, transparent),
                                Color::White => (black, white + 1, transparent),
                                Color::Transparent => (black, white, transparent + 1),
                            },
                        )
                    })
                    .min()
                    .unwrap();
            println!("{}", white * transparent);

            let mut picture_iter = picture.into_iter();
            let uppermost_layer = picture_iter.next().unwrap();
            picture_iter
                .fold(uppermost_layer, |top_layer, bottom_layer| {
                    top_layer
                        .iter()
                        .zip(bottom_layer.iter())
                        .map(|(top_row, bottom_row)| {
                            top_row
                                .iter()
                                .zip(bottom_row.iter())
                                .map(|x| match x {
                                    (Color::Transparent, x) => *x,
                                    (x, _) => *x,
                                })
                                .collect::<Vec<_>>()
                        })
                        .collect::<Vec<_>>()
                })
                .iter()
                .map(|row| row.iter().map(Color::to_char).collect::<String>())
                .for_each(|row| println!("{}", row));
        });
}

#[derive(Clone, Copy, Debug)]
enum Color {
    Black,
    White,
    Transparent,
}

impl Color {
    pub fn to_char(&self) -> char {
        match self {
            Color::Black => ' ',
            Color::White => '█',
            Color::Transparent => '▒',
        }
    }

    pub fn parse(ch: char) -> Color {
        match ch {
            '0' => Color::Black,
            '1' => Color::White,
            '2' => Color::Transparent,
            _ => panic!("invalid Color value"),
        }
    }
}
