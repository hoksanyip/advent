pub fn main() {
    println!(
        "{}",
        include_str!("../../data/day01.txt")
            .split("\n\n")
            .map(|e| e
                .lines()
                .map(|c| c.parse::<u32>().unwrap())
                .sum::<u32>()
            )
            .max()
            .unwrap()
    )
}
