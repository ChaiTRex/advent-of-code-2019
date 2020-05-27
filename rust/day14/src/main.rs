use std::{
    collections::{HashMap, HashSet},
    fs::File,
    hash::Hash,
    io::{BufRead, BufReader},
    str::FromStr,
};

fn main() {
    let recipes = {
        let mut recipes = HashMap::new();

        BufReader::new(File::open("../14.txt").unwrap())
            .lines()
            .for_each(|line| {
                let recipe = line.unwrap().parse::<Recipe>().unwrap();
                recipes.insert(recipe.result().chemical().clone(), recipe);
            });

        recipes
    };

    let sorted_chemicals = topological_sort(&recipes);
    let ore_for_one_fuel = ore_needed(1, &recipes, &sorted_chemicals);
    println!("{}", ore_for_one_fuel);

    let ore_available = 1_000_000_000_000;
    if ore_for_one_fuel > ore_available {
        println!("0");
    } else {
        let mut low_fuel = 1;
        let mut high_fuel = 2;
        let mut high_ore = ore_needed(high_fuel, &recipes, &sorted_chemicals);
        while high_ore <= ore_available {
            low_fuel = high_fuel;
            high_fuel += high_fuel;
            high_ore = ore_needed(high_fuel, &recipes, &sorted_chemicals);
        }
        while high_fuel - low_fuel > 1 {
            let mid_fuel = (low_fuel + high_fuel) / 2;
            let mid_ore = ore_needed(mid_fuel, &recipes, &sorted_chemicals);
            if mid_ore <= ore_available {
                low_fuel = mid_fuel;
            } else {
                high_fuel = mid_fuel;
            }
        }
        println!("{}", low_fuel);
    }
}

#[inline(always)]
fn topological_sort(recipes: &HashMap<Chemical, Recipe>) -> Vec<Chemical> {
    let mut recipes = recipes.clone();

    let mut remaining_chemicals = HashSet::with_capacity(recipes.len() + 1);
    recipes.iter().for_each(|(_, recipe)| {
        let result_chemical = recipe.result().chemical();
        if !remaining_chemicals.contains(result_chemical) {
            remaining_chemicals.insert(result_chemical.clone());
        }
        recipe.ingredients().iter().for_each(|ingredient_quantity| {
            let ingredient_chemical = ingredient_quantity.chemical();
            if !remaining_chemicals.contains(ingredient_chemical) {
                remaining_chemicals.insert(ingredient_chemical.clone());
            }
        });
    });

    let mut result = Vec::with_capacity(remaining_chemicals.len());

    while remaining_chemicals.len() != 0 {
        let chemicals_linked_to = recipes
            .iter()
            .map(|(_, recipe)| {
                recipe
                    .ingredients()
                    .iter()
                    .map(|ingredient_quantity| ingredient_quantity.chemical().clone())
            })
            .flatten()
            .collect::<HashSet<_>>();
        let chemicals_unlinked_to = remaining_chemicals
            .difference(&chemicals_linked_to)
            .cloned()
            .collect::<Vec<_>>();
        chemicals_unlinked_to.into_iter().for_each(|chemical| {
            recipes.remove(&chemical);
            remaining_chemicals.remove(&chemical);
            result.push(chemical.clone());
        });
    }

    result
}

fn ore_needed(
    fuel_desired: u64,
    recipes: &HashMap<Chemical, Recipe>,
    sorted_chemicals: &Vec<Chemical>,
) -> u64 {
    let mut quantities_needed = HashMap::new();
    quantities_needed.insert(Chemical::new("FUEL".to_owned()), fuel_desired as i64);

    for chemical in sorted_chemicals.iter() {
        match recipes.get(&chemical) {
            Some(recipe) => {
                let quantity_needed = quantities_needed.entry(chemical.clone()).or_insert(0);

                if *quantity_needed > 0 {
                    let recipe_result = recipe.result();
                    let recipe_result_quantity = recipe_result.amount() as i64;
                    let reactions_needed =
                        (*quantity_needed + recipe_result_quantity - 1) / recipe_result_quantity;

                    *quantity_needed -= reactions_needed * recipe_result_quantity;
                    recipe.ingredients().iter().for_each(|ingredient| {
                        let quantity_needed = quantities_needed
                            .entry(ingredient.chemical().clone())
                            .or_insert(0);
                        *quantity_needed += reactions_needed * ingredient.amount() as i64;
                    });
                }
            }
            None => (),
        }
    }

    *quantities_needed
        .get(&Chemical::new("ORE".to_owned()))
        .unwrap_or(&0) as u64
}

#[derive(Clone, Debug)]
struct Recipe {
    result: Quantity,
    ingredients: Vec<Quantity>,
}

impl Recipe {
    pub fn result(&self) -> &Quantity {
        &self.result
    }

    pub fn ingredients(&self) -> &Vec<Quantity> {
        &self.ingredients
    }
}

impl FromStr for Recipe {
    type Err = &'static str;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut iter = s.split(" => ");

        let ingredients = match iter.next() {
            None => return Err("empty recipe string"),
            Some(ings) => {
                let mut ingredients = vec![];

                for ing in ings.split(", ") {
                    ingredients.push(match ing.parse() {
                        Err(_) => return Err("invalid ingredient"),
                        Ok(ingredient) => ingredient,
                    });
                }

                ingredients
            }
        };

        let result = match iter.next() {
            None => return Err("no recipe result"),
            Some(rslt) => match rslt.parse() {
                Err(_) => return Err("invalid recipe result"),
                Ok(rslt) => rslt,
            },
        };

        if iter.next().is_some() {
            return Err("has extraneous information after recipe result");
        }

        Ok(Recipe {
            result,
            ingredients,
        })
    }
}

#[derive(Clone, Debug)]
struct Quantity {
    amount: u64,
    chemical: Chemical,
}

impl Quantity {
    pub fn amount(&self) -> u64 {
        self.amount
    }

    pub fn chemical(&self) -> &Chemical {
        &self.chemical
    }
}

impl FromStr for Quantity {
    type Err = &'static str;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut iter = s.split(' ');

        let amount = match iter.next() {
            None => return Err("empty quantity string"),
            Some(amt) => match amt.parse() {
                Err(_) => return Err("invalid amount"),
                Ok(amt) => amt,
            },
        };

        let chemical = match iter.next() {
            None => return Err("no chemical name"),
            Some(name) => Chemical {
                name: name.to_owned(),
            },
        };

        if iter.next().is_some() {
            return Err("has extraneous information after chemical name");
        }

        Ok(Quantity { amount, chemical })
    }
}

#[derive(Clone, Debug, Hash, PartialEq)]
struct Chemical {
    name: String,
}

impl Chemical {
    pub fn new(name: String) -> Self {
        Chemical { name }
    }
}

impl Eq for Chemical {}
