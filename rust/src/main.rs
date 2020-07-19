mod util;
mod problems;
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
            Ok(5) => {
                let eg = fasta::eg_fasta();
                println!("{:?}", eg);
                Ok(())
            }
            Ok(x) => Err(String::from(format!("Error, problem {} note defined", x))),
        }    
    }
}
