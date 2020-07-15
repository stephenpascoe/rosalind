use crate::util::*;


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