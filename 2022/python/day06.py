from typing import Iterable
import itertools

#################################################
# Import
#################################################
with open("2022/data/day06.txt", "r") as f:
    data = "".join(row.strip() for row in f.readlines())


#################################################
# Prepare
#################################################
def rolling_generator(source: Iterable[str], size: int) -> Iterable[str]:
    base = " " + "".join(itertools.islice(source, size - 1))
    for char in source:
        base = base[1:] + char
        yield base


def find_match(data: str, size: int) -> int:
    source = iter(data)
    roll = rolling_generator(source, size)
    found = False
    pos = size - 1
    while not found:
        pos += 1
        marker = next(roll)
        found = len(set(marker)) == size
    return pos


#################################################
# Output
#################################################
print(f"Answer 1: {find_match(data, 4)}")
print(f"Answer 2: {find_match(data, 14)}")
