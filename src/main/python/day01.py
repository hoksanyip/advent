import numpy as np

# Import
with open("data/day01.txt", "r") as f:
    data = np.array([int(row.strip()) for row in f.readlines()])

# Process
lag = 3
num_increases = np.sum(data[lag:] - data[:-lag] > 0)

# output
print(f"{num_increases = }")
