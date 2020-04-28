use std::{ error::Error, fs::File, io::Read, iter::repeat_with };

type Mass = u32;

fn main() {
    let masses = parse_file("1.txt").expect("Could not parse file");
    println!("Fuel needed for module mass: {}", masses.iter().map(fuel_needed_for_mass).sum::<Mass>());
    println!("Fuel needed in total:        {}", masses.iter().map(fuel_needed_in_parts).flatten().sum::<Mass>());
}

fn fuel_needed_for_mass(mass: &Mass) -> Mass {
    if *mass >= 9 {
        *mass/3 - 2
    } else {
        0
    }
}

fn fuel_needed_in_parts(mass: &Mass) -> impl Iterator<Item = Mass> {
    let mut mass = *mass;
    repeat_with(move || { mass = fuel_needed_for_mass(&mass); mass }).take_while(|&mass| mass != 0)
}

fn parse_file(filepath: &str) -> Result<Vec<Mass>, Box<dyn Error>> {
    let mut file = File::open(filepath)?;
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;

    let mut result = vec![];
    for line in contents.lines() {
        result.push(line.trim().parse()?);
    }
    
    Ok(result)
}
