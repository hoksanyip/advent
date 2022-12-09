from typing import Iterable

#################################################
# Import
#################################################
with open("2022/data/day06.txt", "r") as f:
    data = "".join(row.strip() for row in f.readlines())


#################################################
# Prepare
#################################################
def moving_generator(source: Iterable[str], size: int) -> Iterable[str]:
    base = "_" + "".join(next(source) for _ in range(size - 1))
    for char in source:
        base = base[1:] + char
        yield base


def find_match(data: str, size: int) -> int:
    roll = moving_generator(iter(data), size)
    marker = ""
    pos = size - 1
    while not len(set(marker)) == size:
        pos += 1
        marker = next(roll)
    return pos


#################################################
# Output
#################################################
print(f"Answer 1: {find_match(data, 4)}")
print(f"Answer 2: {find_match(data, 14)}")
