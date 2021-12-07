import numpy as np

# Import
with open("data/day07.txt", "r") as f:
    data = np.array([int(row) for row in f.readline().split(",")])

# Prepare
m = max(data)

# Process
min([sum(np.abs(data - i) * (np.abs(data - i) + 1)) / 2 for i in range(m)])
