
use std::io;


pub fn get_stdin_line() -> io::Result<String> {
    let mut input = String::new();

    io::stdin().read_line(&mut input)?;
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

pub fn fibk(n: u32, k: u32) -> u32 {
    if n < 3 {
        1
    }
    else {
        fibk(n-1, k) + 3*fibk(n-2, k)
    }
}

#[test]
fn test_fibk() {
    assert_eq!(fibk(5, 3), 19);
}