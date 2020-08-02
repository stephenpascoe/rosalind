mod util;
mod problems;
mod graph;
mod fasta;

use std::env;

fn main() -> Result<(), String> {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        Err(String::from(format!("usage: {} <problem-code>", &args[0])))
    }
    else {
        match args[1].as_str() {
            "dna" => problems::problem_dna(),
            "rna" => problems::problem_rna(),
            "revc" => problems::problem_revc(),
            "fib" => problems::problem_fib(),
            "gc" => problems::problem_gc(),
            "hamm" => problems::problem_hamm(),
            "iprb" => problems::problem_iprb(),
            //"prot" => problems::problem_prot(),
            "subs" => problems::problem_subs(),
            "cons" => problems::problem_cons(),
            "fibd" => problems::problem_fibd(),
            "grph" => problems::problem_grpy(),
            x => Err(String::from(format!("Error, problem {} note defined", x))),
        }    
    }
}
