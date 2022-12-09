from __future__ import annotations
from enum import Enum
from dataclasses import dataclass
from typing import Set, Tuple, Iterable, TypeVar
import numpy as np

Pos = TypeVar("Pos", bound=Tuple[int, int])


#################################################
# Prepare
#################################################
class Direction(Enum):
    LEFT, RIGHT, UP, DOWN = "LRUD"


@dataclass
class Instruction():
    direction: Direction
    steps: int

    @staticmethod
    def parse(txt: str) -> Instruction:
        direction, steps = txt.strip().split(" ")
        return Instruction(Direction(direction), int(steps))

    def move(self, s: Pos) -> Iterable[Pos]:
        step_range = range(1, self.steps + 1)
        match self.direction:
            case Direction.LEFT: return ((s[0], s[1] - t) for t in step_range)
            case Direction.RIGHT: return ((s[0], s[1] + t) for t in step_range)
            case Direction.UP: return ((s[0] - t, s[1]) for t in step_range)
            case Direction.DOWN: return ((s[0] + t, s[1]) for t in step_range)


#################################################
# Import
#################################################
with open("2022/data/day09.txt", "r") as f:
    data = [Instruction.parse(row) for row in f.readlines()]


#################################################
# Process
#################################################
@dataclass
class Trace():
    pos: Pos
    counter: Set

    def update(self, p: Pos):
        self.pos = p
        self.counter.update({p})


def update_tail(head: Pos, tail: Pos) -> Pos:
    dy = head[0] - tail[0]
    dx = head[1] - tail[1]
    if abs(dx) <= 1 and abs(dy) <= 1:
        # If tail is touching head, then no change needed
        return tail
    else:
        # Move towards head according to sign
        return (tail[0] + np.sign(dy), tail[1] + np.sign(dx))


def simulate_trace(knots: int = 2) -> int:
    traces = [Trace((0, 0), set()) for _ in range(knots)]
    for move in data:
        for head_update in move.move(traces[0].pos):
            # Update head trace
            traces[0].update(head_update)
            # Update tail traces
            for i in range(knots - 1):
                tail_update = update_tail(traces[i].pos, traces[i+1].pos)
                traces[i+1].update(tail_update)

    return len(traces[-1].counter)


#################################################
# Output
#################################################
print(f"Answer 1: {simulate_trace(2)}")
print(f"Answer 2: {simulate_trace(10)}")
