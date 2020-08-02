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
use std::cmp::max;

pub type Fasta = HashMap<String, String>;


pub struct ProfileMatrix {
    n: usize,
    a_count: Vec<u32>,
    c_count: Vec<u32>,
    g_count: Vec<u32>,
    t_count: Vec<u32>
}

impl ProfileMatrix {
    fn new(n: usize) -> ProfileMatrix {
        ProfileMatrix {
            n: n,
            a_count: vec![0; n],
            c_count: vec![0; n],
            g_count: vec![0; n],
            t_count: vec![0; n],
        }
    }

    fn add(&mut self, dna: &String) {
        if dna.len() != self.n  as usize { panic!(format!("Expected dna of size {}", self.n)); }
        let mut i = 0;

        for ch in dna.chars() {
            match ch {
                'A' => self.a_count[i] += 1,
                'C' => self.c_count[i] += 1,
                'G' => self.g_count[i] += 1,
                'T' => self.t_count[i] += 1,
                _   => panic!("Unexpected char in dna")
            };
            i += 1;
        }
    }

    pub fn print(&self) {
        println!("A: {}", self.a_count.iter().map(|x| x.to_string()).collect::<Vec<String>>().join(" "));
        println!("C: {}", self.c_count.iter().map(|x| x.to_string()).collect::<Vec<String>>().join(" "));
        println!("G: {}", self.g_count.iter().map(|x| x.to_string()).collect::<Vec<String>>().join(" "));
        println!("T: {}", self.t_count.iter().map(|x| x.to_string()).collect::<Vec<String>>().join(" "));
    }

    pub fn consensus(&self) -> String {
        let mut dna = String::with_capacity(self.n);

        for i in 0..self.n {
            let bmax = max(self.t_count[i], max(self.g_count[i], max(self.c_count[i], self.a_count[i])));
            dna.push(if self.a_count[i] == bmax { 'A' }
                        else if self.c_count[i] == bmax { 'C' }
                        else if self.g_count[i] == bmax { 'G' }
                        else { 'T' });
        }

        dna
    }
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


pub fn profile_matrix(fasta: Fasta) -> Result<ProfileMatrix, String> {

    let mut pm = ProfileMatrix::new(fasta.values().nth(0).unwrap().len());

    for dna in fasta.values() {
        pm.add(dna);
    }

    Ok(pm)
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

#[test]
fn test_profile_matric() {
    let fasta_eg = concat!(">Rosalind_1\nATCCAGCT\n>Rosalind_2\nGGGCAACT\n>Rosalind_3\nATGGATCT\n",
                           ">Rosalind_4\nAAGCAACC\n>Rosalind_5\nTTGGAACT\n>Rosalind_6\nATGCCATT\n",
                           ">Rosalind_7\nATGGCACT\n");
    let fasta = read_fasta(io::Cursor::new(fasta_eg)).unwrap();
    let pm = profile_matrix(fasta).unwrap();

    assert_eq!(pm.g_count, vec![1, 1, 6, 3, 0, 1, 0, 0]);
    assert_eq!(pm.consensus(), "ATGCAACT".to_string());

}