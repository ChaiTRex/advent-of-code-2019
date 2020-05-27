use criterion::{criterion_group, criterion_main, Benchmark, Criterion};
use intcode::machine::{Machine, MachinePause};
use std::time::Duration;

pub fn criterion_benchmark(c: &mut Criterion) {
    c.bench(
        "fibonacci",
        Benchmark::new("-10 to 10,000", |b| b.iter(fibonacci))
            .nresamples(10000)
            .sample_size(100)
            .warm_up_time(Duration::from_secs(30))
            .measurement_time(Duration::from_secs(600)),
    );
}

fn fibonacci() {
    let mut machine = [
        109, 369, 3, 362, 1101, 0, 0, 363, 1101, 0, 1, 364, 1007, 362, 0, 365, 1006, 365, 180,
        2001, 362, -2, 365, 207, -1, 365, 365, 1005, 365, 51, 22201, -2, -2, 0, 22201, -1, -1, 1,
        109, 2, 2001, 362, -2, 365, 207, -1, 365, 365, 1006, 365, 30, 1208, -2, 1, 365, 1005, 365,
        156, 207, -1, 362, 365, 1006, 365, 109, 2, 363, 363, 366, 102, -1, 363, 365, 1, 365, 364,
        365, 1, 365, 364, 365, 2, 363, 365, 363, 2, 364, 364, 364, 1, 364, 366, 364, 109, -2, 1208,
        -2, 1, 365, 1005, 365, 156, 207, -1, 362, 365, 1005, 365, 65, 2001, 362, -2, 362, 1, 363,
        363, 366, 2, 363, 363, 363, 2, 364, 364, 365, 1, 363, 365, 363, 1, 366, 364, 366, 2, 364,
        366, 364, 109, -2, 1208, -2, 1, 365, 1005, 365, 156, 207, -1, 362, 365, 1005, 365, 65,
        1106, 0, 109, 1008, 362, 0, 365, 1006, 365, 345, 102, -2, 364, 365, 1, 365, 363, 365, 2,
        363, 365, 363, 4, 363, 1106, 0, 2, 2001, 362, -1, 365, 2007, 365, -2, 365, 1005, 365, 212,
        22201, -2, -2, 0, 22201, -1, -1, 1, 109, 2, 2001, 362, -1, 365, 2007, 365, -2, 365, 1006,
        365, 191, 1208, -2, 1, 365, 1005, 365, 317, 2007, 362, -2, 365, 1006, 365, 270, 2, 363,
        363, 366, 102, -1, 363, 365, 1, 365, 364, 365, 1, 365, 364, 365, 2, 363, 365, 363, 2, 364,
        364, 364, 1, 364, 366, 364, 109, -2, 1208, -2, 1, 365, 1005, 365, 317, 2007, 362, -2, 365,
        1005, 365, 226, 2001, 362, -1, 362, 1, 363, 363, 366, 2, 363, 363, 363, 2, 364, 364, 365,
        1, 363, 365, 363, 1, 366, 364, 366, 2, 364, 366, 364, 109, -2, 1208, -2, 1, 365, 1005, 365,
        317, 2007, 362, -2, 365, 1005, 365, 226, 1106, 0, 270, 1008, 362, 0, 365, 1006, 365, 345,
        102, -1, 363, 365, 1, 365, 364, 365, 1, 365, 364, 365, 2, 363, 365, 363, 4, 363, 1106, 0,
        2, 2, 363, 363, 363, 2, 364, 364, 365, 1, 363, 365, 363, 4, 363, 1106, 0, 2, 0, 0, 1, 0, 0,
        1, -1,
    ]
    .iter()
    .copied()
    .collect::<Machine>();

    let mut fibs = Vec::with_capacity(21);
    for i in -10..=10 {
        match machine.execute() {
            Ok(MachinePause::AwaitingInput(new_machine)) => {
                machine = new_machine.push_input(i);
            }
            _ => panic!("machine not awaiting input"),
        }
        match machine.execute() {
            Ok(MachinePause::HasOutput(new_machine, x)) => {
                machine = new_machine;
                fibs.push(x)
            }
            _ => panic!("machine not producing output"),
        }
    }
    assert_eq!(
        fibs,
        vec![-55, 34, -21, 13, -8, 5, -3, 2, -1, 1, 0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55]
    );

    for i in 11..10_000 {
        match machine.execute() {
            Ok(MachinePause::AwaitingInput(new_machine)) => {
                machine = new_machine.push_input(i);
            }
            _ => panic!("machine not awaiting input"),
        }
        match machine.execute() {
            Ok(MachinePause::HasOutput(new_machine, _)) => {
                machine = new_machine;
            }
            _ => panic!("machine not producing output"),
        }
    }

    match machine.execute() {
        Ok(MachinePause::AwaitingInput(new_machine)) => {
            machine = new_machine.push_input(10_000);
        }
        _ => panic!("machine not awaiting input"),
    }
    match machine.execute() {
        Ok(MachinePause::HasOutput(_, -2872092127636481573)) => (),
        _ => panic!("machine acted incorrectly"),
    }
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
