/*
    Adjacency graphs
*/
use crate::fasta::*;


/*
    For the purpose of problem `grph` we only need to consider each pair of nodes and decide 
    whether they form a directed edge.

    We will implement this as an iterator.

    We can increase performance by sorting the records by sequence prefix.

*/

#[derive(PartialOrd, PartialEq, Eq, Ord)]
struct AdjacencyFindItem<'a> {
    prefix: &'a str,
    suffix: &'a str,
    key: &'a String,
}

pub struct AdjacencyIter<'a> {
    items: Vec<AdjacencyFindItem<'a>>,
    src_i: usize,
    dest_i: usize
}

impl <'a> Iterator for AdjacencyIter<'a> {
    type Item = (&'a String, &'a String);

    // TODO : Short circuit for efficiency
    fn next(&mut self) -> Option<(&'a String, &'a String)> {
        for src_j in self.src_i..self.items.len() {
            for dest_j in self.dest_i..self.items.len() {
                if src_j == dest_j { continue; }
                let src = &self.items[src_j];
                let dest = &self.items[dest_j];

                if src.suffix == dest.prefix {
                    // Manually incremement loop counter and store before returning
                    self.src_i = src_j;
                    self.dest_i = dest_j + 1;
                    return Some((src.key, dest.key));
                }
            }
            self.dest_i = 0;
        }
        None
    }
}

fn fasta_to_findlist(fasta: &Fasta, n: usize) -> Vec<AdjacencyFindItem> {
    let mut items: Vec<AdjacencyFindItem> = fasta.iter()
                                                .map(|(k, v)| AdjacencyFindItem {
                                                                prefix: &v[0..n],
                                                                suffix: &v[v.len()-n..],
                                                                key: &k
                                                            } )
                                                .collect();
    items.sort();

    return items;
}

pub fn overlap_edges(fasta: &Fasta, prefix_n: usize) -> AdjacencyIter {
    let items = fasta_to_findlist(fasta, prefix_n);
    
    AdjacencyIter {items, src_i: 0, dest_i: 0}
}

#[test]
fn test_fasta_to_findlist() {
    let fasta_eg = ">Rosalind_42\nTACCGCCATTACGGC\n>Rosalind_924\nGGTGAAAGATCCGCATTAG\n";
    let fasta = read_fasta(io::Cursor::new(fasta_eg)).unwrap();

    let findlist = fasta_to_findlist(&fasta, 3);
    assert_eq!(findlist[0].prefix, "GGT");
    assert_eq!(findlist[0].key, "Rosalind_924");
    assert_eq!(findlist[1].prefix, "TAC");

}

#[test]
fn test_overlap_edges() {
    let fasta_eg = ">Rosalind_0498\nAAATAAA\n>Rosalind_2391\nAAATTTT\n>Rosalind_2323\nTTTTCCC\n>Rosalind_0442\nAAATCCC\n>Rosalind_5013\nGGGTGGG\n";
    let fasta = read_fasta(io::Cursor::new(fasta_eg)).unwrap();

    let edges: Vec<(&String, &String)> = overlap_edges(&fasta, 3).collect();

    let src = "Rosalind_0498".to_string();
    let dest = "Rosalind_2391".to_string();

    for (a, b) in edges.iter() {
        println!("{} {}", a, b);
    }

    assert!(edges.iter().any(|(a, b)| (a == &&src) && (b == &&dest)));


}