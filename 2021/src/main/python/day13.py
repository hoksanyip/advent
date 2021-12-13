import numpy as np
from scipy import sparse

#################################################
# Import
#################################################
with open("2021/src/main/resources/day13.txt", "r") as f:
    x, y, fold = [], [], []
    for row in f.readlines():
        if "," in row:
            # Parse coordinates
            i, j = row.strip().split(",")
            x.append(int(j))
            y.append(int(i))
        elif row.startswith("fold along"):
            # Parse instructions
            axis, loc = row.replace("fold along ", "").split("=")
            fold.append([axis, int(loc)])

#################################################
# Prepare
#################################################
d = np.repeat(1, len(x))
paper = sparse.csr_matrix((d, (x, y))).toarray()

#################################################
# Process
#################################################
for axis, coord in fold:
    # Fold along axis
    axis = ["y", "x"].index(axis)
    part1, _, part2 = np.split(paper, [coord, coord + 1], axis)
    paper = part1 + np.flip(part2, axis)

#################################################
# Output
#################################################
with np.printoptions(formatter={"all": lambda x: "#" if x > 0 else "."}, linewidth=1000):
    print(paper)
