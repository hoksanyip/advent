#################################################
# Import
#################################################
with open("2022/data/day02.txt", "r") as f:
    data = [row.strip() for row in f.readlines()]


#################################################
# Prepare
#################################################
def parse_game1(line: str):
    elf, you = line.split(" ")
    elf = ord(elf) - ord('A')
    you = ord(you) - ord('X')
    win_score = 3 * ((you - elf + 1) % 3)
    rps_score = you + 1
    return win_score + rps_score


def parse_game2(line: str):
    elf, outcome = line.split(" ")
    elf = ord(elf) - ord('A')
    outcome = ord(outcome) - ord('X')
    win_score = outcome * 3
    rps_score = (elf + outcome - 1) % 3 + 1
    return win_score + rps_score


#################################################
# Process
#################################################
output1 = sum(parse_game1(line) for line in data)
output2 = sum(parse_game2(line) for line in data)

#################################################
# Output
#################################################
print(f"Answer 1: {output1}")
print(f"Answer 2: {output2}")
