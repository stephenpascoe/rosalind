use crate::util::*;
use crate::graph::*;
use crate::fasta::*;
use std::io;


type ProblemResult = Result<(), String>;


pub fn problem_dna() -> ProblemResult {
    let line = get_stdin_line()?;

    println!("{} {} {} {}", 
      count_char('A', &line),
      count_char('C', &line),
      count_char('G', &line),
      count_char('T', &line),
    );

    Ok(())
}

pub fn problem_rna() -> ProblemResult {
    let line = get_stdin_line()?;

    println!("{}", transcribe_dna(&line));

    Ok(())
}

pub fn problem_revc() -> ProblemResult {
    let line = get_stdin_line()?;

    println!("{}", reverse_complement(&line));

    Ok(())
}


pub fn problem_fib() -> ProblemResult {
    let line = get_stdin_line()?;
    let nums : Vec<u64> = line.split(" ")
                                .map(|s| s.parse())
                                .filter_map(Result::ok)
                                .collect();
    if nums.len() != 2 {
        return Err(String::from("Expected 2 numbers"))
    }

    println!("{}", fibk(nums[0], nums[1]));

    Ok(())
}

pub fn problem_gc() -> ProblemResult {
    let stdin = io::stdin();
    let fasta = read_fasta(stdin.lock())?;
    let (top_key, top_gc) = fasta.iter()
                                .map(|(k, dna)| (k, gc_content(dna)))
                                .max_by(|(_, gc1), (_, gc2)| 
                                            gc1.partial_cmp(gc2).unwrap())
                                .unwrap();

    print!("{}\n{}\n", top_key, top_gc * 100.0);

    Ok(())
}

pub fn problem_hamm() -> ProblemResult {
    let dna1 = get_stdin_line()?;
    let dna2 = get_stdin_line()?;

    print!("{}\n", hamming_distance(dna1, dna2));

    Ok(())
}

pub fn problem_iprb() -> ProblemResult {
    let vec = get_stdin_line()?.split(" ")
                .map(|x| x.parse::<u64>().expect("failed to parse"))
                .collect::<Vec<u64>>();
    if vec.len() != 3 { return Err(String::from("Expected 3 arguments")); }

    let result = mate_prob(vec[0], vec[1], vec[2]);
    println!("{}", result);

    Ok(())
}

// TODO : PROT missing

pub fn problem_subs() -> ProblemResult {
    let s = get_stdin_line()?;
    let t = get_stdin_line()?;

    for i in 0..s.len() {
        if s[i..].starts_with(&t) {
            print!("{} ", i+1);
        }
    }
    println!("");

    Ok(())
}

pub fn problem_cons() -> ProblemResult {
    let stdin = io::stdin();
    let fasta = read_fasta(stdin.lock())?;

    let pm = profile_matrix(fasta)?;
    println!("{}", pm.consensus());
    pm.print();

    Ok(())
}

pub fn problem_fibd() -> ProblemResult {
    let vec = get_stdin_line()?.split(" ")
                .map(|x| x.parse::<u32>().expect("failed to parse"))
                .collect::<Vec<u32>>();

    println!("{}", mortal_fib(vec[0], vec[1]));

    Ok(())
}

pub fn problem_grpy() -> ProblemResult {
    let stdin = io::stdin();
    let fasta = read_fasta(stdin.lock())?;

    for (src, dest) in overlap_edges(&fasta, 3) {
        println!("{} {}", src, dest)
    }

    Ok(())
}