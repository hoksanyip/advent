from collections import Counter
import numpy as np

#################################################
# Import
#################################################
with open("src/main/resources/day06.txt", "r") as f:
    data = Counter([int(v) for v in f.readline().split(",")])

#################################################
# Prepare
#################################################
n, m = 256, 7
pop = np.vstack([data.get(i, 0) for i in range(m + 2)])
# (Transition matrix)
A = np.roll(np.diag(np.ones(m + 2)), 1, axis=1)
A[m - 1, 0] = 1

#################################################
# Process
#################################################
population = np.linalg.matrix_power(A, n).dot(pop).sum()

#################################################
# Output
#################################################
print(f"{population = }")
