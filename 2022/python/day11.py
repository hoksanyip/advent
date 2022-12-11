from __future__ import annotations
import copy
from dataclasses import dataclass
from io import TextIOWrapper
import math
import re
from typing import Callable, List, Tuple


#################################################
# Prepare
#################################################
@dataclass
class RegexMatch(str):
    """
    Regular expression pattern matching helper class

    This class is used to be able to combine regular expression
    in the pattern matching functionality of Python (requires Python >= 3.10).
    """
    string: str
    match: re.Match = None

    def __eq__(self, pattern: str) -> bool:
        """
        Equal comparison function.

        :param pattern: Regular expression
        :return: Boolean indicating whether match has been found.   
        """
        self.match = re.search(pattern, self.string)
        return self.match is not None

    def __getitem__(self, group):
        """
        Get item of match results.

        :param group: Index of re.match function output, first output is the complete text,
                      so usually index of groups starts with 1 instead.
        :return: Matched string based on index number.
        """
        return self.match[group]


@dataclass
class Monkey():
    id: int
    items: List[int]
    operation: Callable  # function to increase
    denominator: int
    target: Tuple[int, int]
    counter: int = 0

    @staticmethod
    def parse(f: TextIOWrapper) -> List[Monkey]:
        monkeys = []

        while line := f.readline():
            match RegexMatch(line.strip()):
                case "Monkey (\d+):" as _id: id = int(_id[1])
                case "Starting items: (.*)" as _items: items = [int(item.strip()) for item in _items[1].split(",")]
                case "Operation: new = (.*)" as _operation: operation = eval(f"lambda old: {_operation[1]}")
                case "Test: divisible by (\d+)" as _denom: denominator = int(_denom[1])
                case "If true: throw to monkey (\d+)" as monkey: target1 = int(monkey[1])
                case "If false: throw to monkey (\d+)" as monkey: target0 = int(monkey[1])
                case _:
                    target = (target0, target1)
                    monkey = Monkey(id, items, operation, denominator, target)
                    monkeys.append(monkey)

        # Add latest monkey (as last line does not fall under "case else" condition)
        target = (target0, target1)
        monkey = Monkey(id, items, operation, _denom, target)
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
            target = self.target[1] if worry % self.denominator == 0 else self.target[0]
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
    """
    Simulate a number of rounds of monkey businesses.
    """
    # Avoid changing the source data (by reference) in order to fix side effect problem
    monkeys = copy.deepcopy(monkeys)
    # Simulate the rounds
    for _ in range(rounds):
        for monkey in monkeys:
            monkey.inspect(monkeys, decrease)

    # Calculate the monkey business value
    second, first = sorted([m.counter for m in monkeys])[-2:]
    return first * second


output1 = simulate_rounds(data, 20, decrease=lambda x: x // 3)
denominator = math.prod([m.denominator for m in data])
output2 = simulate_rounds(data, 10000, lambda x: x % denominator)


#################################################
# Output
#################################################
print(f"Answer 1: {output1}")
print(f"Answer 2: {output2}")
