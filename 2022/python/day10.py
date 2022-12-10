from __future__ import annotations
from dataclasses import dataclass
from typing import Iterable, List
from enum import Enum
import numpy as np


#################################################
# Prepare
#################################################
class Command(Enum):
    NOOP, ADDX = "noop", "addx"


@dataclass
class Instruction():
    command: str
    amount: int = 0

    @staticmethod
    def parse(txt: str) -> Instruction:
        match txt.strip().split(" "):
            case "noop", : return Instruction(Command.NOOP, 0)
            case "addx", amount: return Instruction(Command.ADDX, int(amount))

    def execute(self, x: int) -> Iterable[int]:
        match self.command:
            case Command.NOOP: return (x,)
            case Command.ADDX: return (x, x + self.amount)


def enumerate_instructions(data: List[Instruction]) -> Iterable[int]:
    x = 1
    for instruction in data:
        for x in instruction.execute(x):
            yield x


#################################################
# Import
#################################################
with open("2022/data/day10.txt", "r") as f:
    data = [Instruction.parse(row) for row in f.readlines()]

#################################################
# Process
#################################################
NROW, NCOL = 6, 40

output = [1] + [instr for instr in enumerate_instructions(data)][:-1]
output1 = sum(i * output[i - 1] for i in range(20, 240, 40))

output = np.reshape(output, (NROW, NCOL))
sprite = np.tile(np.arange(NCOL), (NROW, 1))
output2 = np.abs(output - sprite) <= 1

#################################################
# Output
#################################################
print(f"Answer 1: {output1}")

print("Answer 2:")
with np.printoptions(formatter={"all": lambda x: "\u2588" if x > 0 else " "}, linewidth=1000):
    print(output2)
