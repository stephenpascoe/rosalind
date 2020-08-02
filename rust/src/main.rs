mod util;
mod problems;
mod graph;
mod fasta;

use std::env;

fn main() -> Result<(), String> {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        Err(String::from(format!("usage: {} <problem-no>", &args[0])))
    }
    else {
        match args[1].parse::<u32>() {
            Err(_) => Err(String::from(format!("Error, non numeric argument {:?}", &args[1]))),
            Ok(1) => problems::problem_1(),
            Ok(2) => problems::problem_2(),
            Ok(3) => problems::problem_3(),
            Ok(4) => problems::problem_4(),
            Ok(5) => problems::problem_5(),
            Ok(6) => problems::problem_6(),
            Ok(7) => problems::problem_7(),
            Ok(8) => problems::problem_8(),
            Ok(9) => problems::problem_9(),
            Ok(10) => problems::problem_10(),
            Ok(11) => problems::problem_11(),
            Ok(x) => Err(String::from(format!("Error, problem {} note defined", x))),
        }    
    }
}
