import numpy as np
from typing import Callable

#################################################
# Import
#################################################
with open("src/main/resources/day03.txt", "r") as f:
    data = np.array([int("0b" + row, 2) for row in f.readlines()])

#################################################
# Prepare
#################################################
k = int(np.ceil(np.log2(max(data))))
def find_common(data: np.array, k: int, f: Callable) -> int:
    data_iter = data.copy()
    acc = 0
    for i in range(k):
        value = 2 ** (k - i - 1)
        validation = data_iter >= value
        which = f(np.mean(validation))
        acc += value * which
        selection = validation * which + (1 - validation) * (1 - which)
        data_iter = data_iter[selection.astype(bool)] - which * value
    return acc

#################################################
# Process
#################################################
oxygen = find_common(data, k, lambda x: x >= 0.5)
scrubber = find_common(data, k, lambda x: (0 < x < 0.5) | (x == 1))

#################################################
# Output
#################################################
print(f"{oxygen * scrubber = }")
