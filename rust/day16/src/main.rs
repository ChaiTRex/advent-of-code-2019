use itertools::repeat_n;
use std::{
    fs::File,
    io::{BufRead, BufReader},
};

fn main() {
    BufReader::new(File::open("../16.txt").unwrap())
        .lines()
        .for_each(|line| {
            let digits = line
                .unwrap()
                .chars()
                .map(|ch| match ch {
                    '0'..='9' => ch as i8 - '0' as i8,
                    _ => panic!("non-numeric character"),
                })
                .collect::<Vec<_>>();

            {
                let mut digits = digits.clone();

                (0..100).for_each(|_| {
                    digits = apply_phase(&digits);
                });
                println!(
                    "{}{}{}{}{}{}{}{}",
                    digits[0],
                    digits[1],
                    digits[2],
                    digits[3],
                    digits[4],
                    digits[5],
                    digits[6],
                    digits[7]
                );
            }

            {
                let offset = 1_000_000 * digits[0] as usize
                    + 100_000 * digits[1] as usize
                    + 10_000 * digits[2] as usize
                    + 1_000 * digits[3] as usize
                    + 100 * digits[4] as usize
                    + 10 * digits[5] as usize
                    + digits[6] as usize;
                let mut digits = repeat_n(digits, 10_000).flatten().collect::<Vec<_>>();

                (0..100).for_each(|_| {
                    digits = apply_phase(&digits);
                });
                let digits = &digits[offset..offset + 8];
                println!(
                    "{}{}{}{}{}{}{}{}",
                    digits[0],
                    digits[1],
                    digits[2],
                    digits[3],
                    digits[4],
                    digits[5],
                    digits[6],
                    digits[7]
                );
            }
        });
}

fn apply_phase(digits: &[i8]) -> Vec<i8> {
    (1..=digits.len())
        .map(|stride_len| {
            let mut chunks = digits[stride_len - 1..].chunks(stride_len);
            let mut sum = 0;
            loop {
                match chunks.next() {
                    Some(xs) => sum += xs.iter().map(|x| *x as i32).sum::<i32>(),
                    None => break,
                }
                if chunks.next().is_none() {
                    break;
                }
                match chunks.next() {
                    Some(xs) => sum -= xs.iter().map(|x| *x as i32).sum::<i32>(),
                    None => break,
                }
                if chunks.next().is_none() {
                    break;
                }
            }
            (sum.abs() % 10) as i8
        })
        .collect::<Vec<_>>()
}
