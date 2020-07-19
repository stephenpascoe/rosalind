/*
    A simple FASTA parser for Rosalind FASTA format.

    DNA strings must be labeled when they are consolidated into a database. 
    A commonly used method of string labeling is called FASTA format. 
    In this format, the string is introduced by a line that begins with '>', followed
    by some labeling information. Subsequent lines contain the string itself; 
    the first line to begin with '>' indicates the label of the next string.
    
    In Rosalind's implementation, a string in FASTA format will be labeled by the 
    ID "Rosalind_xxxx", where "xxxx" denotes a four-digit code between 0000 and 9999.
*/

extern crate regex;

use std::collections::HashMap;
use std::io;
use regex::Regex;

type Fasta = HashMap<i32, String>;

pub fn eg_fasta() -> Fasta {
    let mut eg = Fasta::new();
    eg.insert(42, "AACCGCCATTACGGC".to_string());
    eg.insert(924, "GGTGAAAGATCCGCATTAG".to_string());
    eg
}

pub fn read_fasta(bufreader: impl io::BufRead) -> Result<Fasta, String> {
    let mut fasta = Fasta::new();
    let re = Regex::new(r"Rosalind_(\d+)\n((?s).*)").unwrap();

    for chunk_r in bufreader.split(b'>') {
        match chunk_r {
            Ok(chunk) => {
                if chunk.len() == 0 { continue; }
                let chunk_str = match std::str::from_utf8(&chunk) {
                    Ok(s) => s,
                    Err(_) => { return Err("UTF Decoding error".to_string()); }
                };
                for cap in re.captures_iter(chunk_str) {
                    let key: i32 = cap[1].parse().expect("Can't parse key");
                    let sequence = cap[2].to_string().lines().collect::<Vec<_>>().concat();
                    fasta.insert(key, sequence);
                };
            }
            Err(_) => { return Err("IO Error".to_string()); }
        }
    };
    Ok(fasta)
}

#[test]
fn test_read_fasta_1() {
    let fasta_eg = ">Rosalind_42\nAACCGCCATTACGGC\n>Rosalind_924\nGGTGAAAGATCCGCATTAG\n";
    let fasta = read_fasta(io::Cursor::new(fasta_eg)).unwrap();
    assert_eq!(fasta.get(&42).unwrap(), &"AACCGCCATTACGGC".to_string());
}

#[test]
fn test_read_fasta_2() {
    let fasta_eg = ">Rosalind_42\nAACCGCCATTACGGC\n>Rosalind_924\nGGTGAAAGA\nTCCGCATTAG\n";
    let fasta = read_fasta(io::Cursor::new(fasta_eg)).unwrap();
    assert_eq!(fasta.get(&924).unwrap(), &"GGTGAAAGATCCGCATTAG".to_string());
}