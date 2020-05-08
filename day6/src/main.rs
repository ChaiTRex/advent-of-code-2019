use std::{collections::HashMap, fs::File, io::Read};

fn main() {
    let tree;
    {
        let mut f = File::open("6.txt").unwrap();
        let mut contents = String::new();
        f.read_to_string(&mut contents).unwrap();

        let mut map = HashMap::new();
        contents.lines().for_each(|orbit| {
            let mut v = orbit.split(')').map(String::from).collect::<Vec<_>>();
            let orbiter = v.pop().unwrap();
            let orbitee = v.pop().unwrap();
            map.insert(orbiter, orbitee);
        });

        tree = map;
    }

    println!(
        "{}",
        tree.keys()
            .map(|orbiter| path_to_root(&tree, orbiter).len() - 1)
            .sum::<usize>()
    );

    let path_to_you = path_to_root(&tree, "YOU");
    let path_to_san = path_to_root(&tree, "SAN");

    let mut to_you = path_to_you.iter().rev();
    let mut to_san = path_to_san.iter().rev();

    while to_you.next() == to_san.next() {}
    println!("{}", to_you.count() + to_san.count());
}

fn path_to_root<'a>(tree: &'a HashMap<String, String>, mut node: &'a str) -> Vec<&'a str> {
    let mut v = vec![node];
    loop {
        node = match tree.get(node) {
            Some(parent) => parent,
            None => return v,
        };
        v.push(node);
    }
}
