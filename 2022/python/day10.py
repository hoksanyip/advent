from __future__ import annotations
from dataclasses import dataclass
import numpy as np
from typing import Iterable


#################################################
# Prepare
#################################################
@dataclass
class Instruction():
    cycles: int = 0
    amount: int = 0

    @staticmethod
    def parse(txt: str) -> Instruction:
        match txt.strip().split(" "):
            case "noop", : return Instruction(1, 0)
            case "addx", amount: return Instruction(2, int(amount))


def execute_instructions(data: Iterable[Instruction]) -> Iterable[int]:
    # Starting point at 1
    x = 1
    for instruction in data:
        # Return per instruction the current amount
        for _ in range(instruction.cycles):
            yield x
        # Update only at end of cycles
        x += instruction.amount


#################################################
# Import
#################################################
with open("2022/data/day10.txt", "r") as f:
    data = (Instruction.parse(row) for row in f.readlines())

#################################################
# Process
#################################################
NROW, NCOL = 6, 40

output = [instr for instr in execute_instructions(data)]
output1 = sum(i * output[i - 1] for i in range(20, 240, 40))

output2 = np.abs(
    np.reshape(output, (NROW, NCOL)) -
    np.tile(np.arange(NCOL), (NROW, 1))
) <= 1

#################################################
# Output
#################################################
print(f"Answer 1: {output1}")

print("Answer 2:")
CHARS = [" " * 2, "\u2588" * 2]
for row in output2:
    print("".join(CHARS[int(x)] for x in row))
