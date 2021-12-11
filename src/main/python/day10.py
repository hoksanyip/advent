from functools import reduce
import numpy as np

#################################################
# Import
#################################################
with open("src/main/resources/day10.txt", "r") as f:
    data = [row.strip() for row in f.readlines()]

#################################################
# Process
#################################################
SCORE = {")": -3, "]": -57, "}": -1197, ">": -25137,
        "(": 1, "[": 2, "{": 3, "<": 4}
MAPPING = {")": "(", "]": "[", "}": "{", ">": "<"}

def process_syntax(row: str) -> int:
    inception = []
    for el in list(row):
        if el in ["(", "[", "{", "<"]:
            inception += el
        elif MAPPING.get(el) == inception[-1]:
            inception = inception[:-1]
        else:
            return SCORE[el]

    scores = [SCORE.get(i) for i in inception]
    score = reduce(lambda x, y: x * 5 + y, reversed(scores))

    return score

#################################################
# Process
#################################################
scores = [process_syntax(row) for row in data]
scores = [s for s in scores if s > 0]

#################################################
# output
#################################################
print(f"{np.median(scores) = }")
