
use std::io;
use statrs::function::factorial::binomial;
use std::collections::VecDeque;

pub fn get_stdin_line() -> Result<String, String> {
    let mut input = String::new();

    match io::stdin().read_line(&mut input) {
        Ok(_) => (),
        Err(_) => {return Err(String::from("IO Error")); }
    }
    while input.ends_with("\n") {
        input.truncate(input.len() - 1);
    }

    Ok(input)
}

pub fn count_char(ch1: char, input: &String) -> i32 {
    let mut count = 0;
    for ch2 in input.chars() {
        if ch2 == ch1 {
            count += 1;
        }
    }
    count
}

pub fn transcribe_dna(dna: &String) -> String {
    dna.replace("T", "U")
}

pub fn reverse_complement(dna: &String) -> String {
    let mut compl = String::with_capacity(dna.len());
    for ch in dna.chars().rev() {
        let ch1 = match ch {
            'G' => 'C',
            'T' => 'A',
            'C' => 'G', 
            'A' => 'T',
            _   => ch,
        };
        compl.push(ch1);
    }
    compl
}

pub fn fibk(n: u64, k: u64) -> u64 {
    if n < 3 {
        1
    }
    else {
        fibk(n-1, k) + k*fibk(n-2, k)
    }
}

/*  Calculate the number of rabit pairs after n months, starting from 1 pair, where each pair lives m months.
    Record the number of pares of each generation, mutate until generation n calculated.
*/
pub fn mortal_fib(n: u32, m: u32) -> u64 {
    let mut fibq: VecDeque<u64> = vec![0; m as usize - 1].into_iter().collect();
    fibq.push_front(1);
    for _i in 1..n {
        // All generations after first reproduce
        fibq.push_front(fibq.iter().skip(1).sum());
        // Oldest generation generation dies
        fibq.pop_back();
    }
    fibq.iter().sum()
}

pub fn gc_content(dna: &String) -> f32 {
    let mut gc: u32 = 0;
    for c in dna.chars() {
        if c == 'G' || c == 'C' {
            gc += 1;
        }
    }
    gc as f32 / dna.len() as f32
}

pub fn hamming_distance(dna1: String, dna2: String) -> usize {
    dna1.chars().zip(dna2.chars()).filter(|(c1, c2)| c1 != c2).count()
}

/* Return the probability of a dominant allene being present for a random mating pair
   given the populations of different allene types
*/
pub fn mate_prob(k: u64, m: u64, n: u64) -> f64 {
    let total = k + m + n;
    let rr = binomial(n, 2) / binomial(total, 2);
    let hh = binomial(m, 2) / binomial(total, 2);
    let hr = (binomial(n, 1) * binomial(m, 1)) / binomial(total, 2);

    1.0 - (rr + hh * 0.25 + hr * 0.5)
}

#[test]
fn test_fibk() {
    assert_eq!(fibk(5, 3), 19);
}

#[test]
fn test_gc_content() {
    let gc = gc_content(&String::from("CCACCCTCGTGGTATGGCTAGGCATTCAGGAACCGGAGAACGCTTCAGACCAGCCCGGACTGGGAACCTGCGGGCAGTAGGTGGAAT"));
    assert_eq!(gc, 0.60919540);
}

#[test]
fn test_mate_prob() {
    assert!(mate_prob(2 , 2, 2) - 0.78333 < 0.00001);
}

#[test]
fn test_mortal_fib() {
    assert_eq!(mortal_fib(6, 3), 4);
}

#[test]
fn test_mortal_fib2() {
    assert_eq!(mortal_fib(7, 3), 5);
}