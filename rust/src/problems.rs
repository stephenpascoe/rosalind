use crate::util::*;


pub fn problem_3() -> Result<(), String>{
    let line = match get_stdin_line() {
        Ok(a) => a,
        Err(_) => { return Err(String::from("IO Error")); }
    };

    println!("{}", reverse_complement(&line));

    Ok(())
}

pub fn problem_1() -> Result<(), String> {
    let line = match get_stdin_line() {
        Ok(a) => a,
        Err(_) => { return Err(String::from("IO Error")); }
    };

    println!("{} {} {} {}", 
      count_char('A', &line),
      count_char('C', &line),
      count_char('G', &line),
      count_char('T', &line),
    );

    Ok(())
}

pub fn problem_2() -> Result<(), String> {
    let line = match get_stdin_line() {
        Ok(a) => a,
        Err(_) => { return Err(String::from("IO Error")); }
    };

    println!("{}", transcribe_dna(&line));

    Ok(())
}
