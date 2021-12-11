from functools import reduce

#################################################
# Import
#################################################
with open("src/main/resources/day08.txt", "r") as f:
    data = [row.strip().split(" | ") for row in f.readlines()]

#################################################
# Prepare
#################################################
class Digit:
    def __init__(self, parser: str):
        digits = [set(d) for d in parser.split(" ")]
        digits.sort(key=len)
        d1 = digits[0]
        d7 = digits[1]
        d4 = digits[2]
        d8 = digits[9]
        d3 = [d for d in digits if len(d) == 5 and d7.issubset(d)][0]
        d5 = [d for d in digits if len(d) == 5 and (d4 - d1).issubset(d)][0]
        d2 = [d for d in digits if len(d) == 5 and not d.issubset(set.union(d4, d5))][0]
        d6 = [d for d in digits if len(d) == 6 and not d1.issubset(d)][0]
        d9 = [d for d in digits if len(d) == 6 and set.union(d3, d4).issubset(d)][0]
        d0 = [d for d in digits if len(d) == 6 and not (d4 - d1).issubset(d)][0]
        self.order = [d0, d1, d2, d3, d4, d5, d6, d7, d8, d9]

    def parse(self, value: str) -> int:
        digits = [self.order.index(set(d)) for d in value.split(" ")]
        return reduce(lambda x, y: x * 10 + y, digits)

#################################################
# Process
#################################################
numbers = [Digit(parser).parse(value) for parser, value in data]

#################################################
# Output
#################################################
print(f"{sum(numbers) = }")
