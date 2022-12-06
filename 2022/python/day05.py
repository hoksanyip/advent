import io
from typing import Dict, List
import string
from collections import namedtuple

Task = namedtuple("Task", ["amount", "start", "end"])


#################################################
# Import
#################################################
def read_stacks(source: io.TextIOWrapper) -> List[List[str]]:
    stacks = {idx + 1: '' for idx in range(9)}
    while line := source.readline():
        # End of stacks description, return result
        if line.strip() == "":
            return stacks

        line = line.replace("\n", "")
        for idx, box in enumerate(line):
            # assume equal spacing and extract only box labels
            if idx % 4 == 1 and box in string.ascii_uppercase:
                stack_id = idx // 4
                stacks[stack_id + 1] = box + stacks[stack_id + 1]


def read_instructions(source: io.TextIOWrapper):
    while line := source.readline():
        _, amount, _, start, _, end = line.split(" ")
        yield Task(int(amount), int(start), int(end))


with open("2022/data/day05.txt", "r") as f:
    stacks = read_stacks(f)
    instructions = list(read_instructions(f))


#################################################
# Process
#################################################
def process_instructions(stacks: Dict[int, str], instructions: List[Task], version: int) -> str:
    stacks = stacks.copy()
    for task in instructions:
        to_move = stacks[task.start][-task.amount:]
        if version == 1:
            to_move = to_move[::-1]
        stacks[task.start] = stacks[task.start][:-task.amount]
        stacks[task.end] += to_move
    top = "".join([stack[-1:] for _, stack in stacks.items()])
    return top


output1 = process_instructions(stacks, instructions, 1)
output2 = process_instructions(stacks, instructions, 2)


#################################################
# Output
#################################################
print(f"Answer 1: {output1}")
print(f"Answer 2: {output2}")
