import string
from typing import List
import itertools
import functools

PRIORITIES = string.ascii_letters


#################################################
# Import
#################################################
with open("2022/src/main/resources/day03.txt", "r") as f:
    data = [row.strip() for row in f.readlines()]


#################################################
# Prepare
#################################################
def convert_to_prio(array: str) -> List[int]:
    return set(PRIORITIES.index(item) + 1 for item in array)


def find_common(x: set, y: set) -> set:
    return x & y


#################################################
# Process
#################################################
def process_sack(array: str):
    half = len(array) // 2
    comps = [array[0:half], array[half:]]
    prios = [convert_to_prio(comp) for comp in comps]
    result = functools.reduce(find_common, prios)
    return result.pop()


def stream_per_three(data: List[str]):
    it = iter(data)
    while (batch := list(itertools.islice(it, 3))):
        yield batch


def process_group(group: List[str]):
    prios = [set(convert_to_prio(elf)) for elf in group]
    result = functools.reduce(find_common, prios)
    return result.pop()


#################################################
# Output
#################################################
# Answer 1
print(f"Answer 1: {sum(process_sack(sack) for sack in data)}")

# Answer 2
print(
    f"Answer 2: {sum([process_group(group) for group in stream_per_three(data)])}")
