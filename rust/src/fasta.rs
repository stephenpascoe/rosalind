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

type Fasta = HashMap<String, String>;

pub fn eg_fasta() -> Fasta {
    let mut eg = Fasta::new();
    eg.insert(String::from("Rosalind_42"), "AACCGCCATTACGGC".to_string());
    eg.insert(String::from("Rosalind_924"), "GGTGAAAGATCCGCATTAG".to_string());
    eg
}

pub fn read_fasta(bufreader: impl io::BufRead) -> Result<Fasta, String> {
    let mut fasta = Fasta::new();

    for chunk_r in bufreader.split(b'>') {
        match chunk_r {
            Ok(chunk) => {
                if chunk.len() == 0 { continue; }
                let chunk_str = match std::str::from_utf8(&chunk) {
                    Ok(s) => s,
                    Err(_) => { return Err("UTF Decoding error".to_string()); }
                };
                let parts = chunk_str.lines().collect::<Vec<_>>();
                if parts.len() < 2 { return Err("Parse Error".to_string()); }
                let key = String::from(parts[0]);
                let sequence = parts[1..].concat();
                fasta.insert(key, sequence);
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
    assert_eq!(fasta.get(&"Rosalind_42".to_string()).unwrap(), &"AACCGCCATTACGGC".to_string());
}

#[test]
fn test_read_fasta_2() {
    let fasta_eg = ">Rosalind_42\nAACCGCCATTACGGC\n>Rosalind_924\nGGTGAAAGA\nTCCGCATTAG\n";
    let fasta = read_fasta(io::Cursor::new(fasta_eg)).unwrap();
    assert_eq!(fasta.get(&"Rosalind_924".to_string()).unwrap(), &"GGTGAAAGATCCGCATTAG".to_string());
}