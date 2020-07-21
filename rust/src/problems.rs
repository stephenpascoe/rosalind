use crate::util::*;
use crate::fasta::*;
use std::io;

use std::collections::HashMap;

pub fn problem_3() -> Result<(), String>{
    let line = get_stdin_line()?;

    println!("{}", reverse_complement(&line));

    Ok(())
}

pub fn problem_1() -> Result<(), String> {
    let line = get_stdin_line()?;

    println!("{} {} {} {}", 
      count_char('A', &line),
      count_char('C', &line),
      count_char('G', &line),
      count_char('T', &line),
    );

    Ok(())
}

pub fn problem_2() -> Result<(), String> {
    let line = get_stdin_line()?;

    println!("{}", transcribe_dna(&line));

    Ok(())
}

pub fn problem_4() -> Result<(), String> {
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

pub fn problem_5() -> Result<(), String> {
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