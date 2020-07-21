
use std::io;


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

#[test]
fn test_fibk() {
    assert_eq!(fibk(5, 3), 19);
}

#[test]
fn test_gc_content() {
    let gc = gc_content(&String::from("CCACCCTCGTGGTATGGCTAGGCATTCAGGAACCGGAGAACGCTTCAGACCAGCCCGGACTGGGAACCTGCGGGCAGTAGGTGGAAT"));
    assert_eq!(gc, 0.60919540);
}