use std::env::args;

mod day01;

fn main() {

    let day: u32 = args()
        .nth(1)
        .unwrap()
        .parse::<u32>()
        .unwrap();
    
    match day {
        1 => day01::main(),
        _ => panic!("Cannot find day")
    }
}
