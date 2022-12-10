pub fn main() {

    fn parse_int(text: &str) -> u32 {
        return text.parse::<u32>().unwrap()
    }

    fn parse_lines(content: &str) -> u32 {
        return content
            .lines()
            .map(parse_int)
            .sum::<u32>()
    }
  
    let output1 = include_str!("../../data/day01.txt")
        .split("\n\n")
        .map(parse_lines)
        .max()
        .unwrap();

    println!("Answer 1: {output1}")
}
