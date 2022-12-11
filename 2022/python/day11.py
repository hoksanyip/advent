from __future__ import annotations
from dataclasses import dataclass
import copy
import re
from typing import Callable, List, Tuple
from io import TextIOWrapper
import numpy as np


#################################################
# Prepare
#################################################
@dataclass
class RegexMatch(str):
    string: str
    match: re.Match = None

    def __eq__(self, pattern):
        self.match = re.search(pattern, self.string)
        return self.match is not None

    def __getitem__(self, group):
        return self.match[group]


@dataclass
class Monkey():
    id: int
    items: List[int]
    operation: Callable
    division: int
    target: Tuple[int, int]
    counter: int = 0

    @staticmethod
    def parse(f: TextIOWrapper) -> List[Monkey]:
        monkeys = []

        while line := f.readline():
            match RegexMatch(line.strip()):
                case "Monkey (\d+):" as _id:
                    id = int(_id[1])
                case "Starting items: (.*)" as _items:
                    items = [int(item.strip())
                             for item in _items[1].split(",")]
                case "Operation: new = (.*)" as _operation:
                    operation = eval(f"lambda old: {_operation[1]}")
                case "Test: divisible by (\d+)" as _division:
                    division = int(_division[1])
                case "If true: throw to monkey (\d+)" as monkey:
                    target1 = int(monkey[1])
                case "If false: throw to monkey (\d+)" as monkey:
                    target0 = int(monkey[1])
                case _:
                    monkey = Monkey(id, items, operation,
                                    division, (target0, target1))
                    monkeys.append(monkey)

        monkey = Monkey(id, items, operation, division, (target0, target1))
        monkeys.append(monkey)
        return monkeys

    def inspect(self, monkeys: List[Monkey], decrease: Callable) -> None:
        # Extract items
        items = self.items
        self.items = []
        # Inspect items
        for item in items:
            self.counter += 1
            worry = decrease(self.operation(item))
            target = self.target[1] if worry % self.division == 0 else self.target[0]
            # Throw to next monkey
            monkeys[target].items.append(worry)
        return


#################################################
# Import
#################################################
with open("2022/data/day11.txt", "r") as f:
    data = Monkey.parse(f)


#################################################
# Process
#################################################
def simulate_rounds(monkeys: List[Monkey], rounds: int, decrease: Callable) -> int:
    for _ in range(rounds):
        for monkey in monkeys:
            monkey.inspect(monkeys, decrease)

    counters = [m.counter for m in monkeys]
    counters.sort()
    business = np.prod(counters[-2:])
    return business


output1 = simulate_rounds(copy.deepcopy(data), 20, lambda x: x // 3)

division = np.prod([m.division for m in data])
output2 = simulate_rounds(copy.deepcopy(data), 10000, lambda x: x % division)


#################################################
# Output
#################################################
print(f"Answer 1: {output1}")
print(f"Answer 2: {output2}")
