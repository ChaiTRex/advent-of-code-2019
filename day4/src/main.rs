use itertools::Itertools;
use std::{fs::File, io::Read};

fn main() {
    let low;
    let high;
    {
        let mut f = File::open("4.txt").unwrap();
        let mut input = String::new();
        let _ = f.read_to_string(&mut input);

        let mut iter = input.split('-');
        low = iter.next().unwrap().trim().parse::<u32>().unwrap();
        high = iter.next().unwrap().trim().parse::<u32>().unwrap();
    }

    let values = (low..=high)
        .map(|x| x.to_string())
        .filter(|x| x.chars().zip(x.chars().skip(1)).all(|(a, b)| a <= b))
        .map(|x| {
            x.chars()
                .group_by(|x| *x)
                .into_iter()
                .map(|(_, xs)| xs.count())
                .collect::<Vec<_>>()
        })
        .collect::<Vec<_>>();

    println!(
        "{}",
        values.iter().filter(|x| x.iter().any(|x| *x >= 2)).count()
    );
    println!(
        "{}",
        values.iter().filter(|x| x.iter().any(|x| *x == 2)).count()
    );
}
