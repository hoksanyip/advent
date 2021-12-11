import numpy as np

#################################################
# Import
#################################################
with open("src/main/resources/day11.txt", "r") as f:
    data = np.array([list(row.strip()) for row in f.readlines()], dtype=int)

#################################################
# Prepare
#################################################
n, _ = data.shape
area = np.eye(n, dtype=int)
area[1:] += np.eye(n, dtype=int)[:-1]
area[:, 1:] += np.eye(n, dtype=int)[:, :-1]

#################################################
# Process
#################################################
num_flashes = 0
t = 0
while not (data == 0).all():
    # Perform step
    t += 1
    data += 1
    # Check flashes
    has_flash = new_flash = data > 9
    while new_flash.any():
        # Update existing flashes
        has_flash = (data > 9)
        # Flash to neighbours
        data += area @ new_flash.astype(int) @ area
        # Check for new flashes
        new_flash = (data > 9) & (~has_flash)
    # Set flashed ones (if any) to zero
    data[has_flash] = 0
    # Count number of flashes so far
    num_flashes += has_flash.sum()

#################################################
# Output
#################################################
print(f"{num_flashes = }")
print(f"{t = }")
