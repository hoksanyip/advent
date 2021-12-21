import numpy as np

#################################################
# Import
#################################################
with open("2021/src/main/resources/day21.txt", "r") as f:
    data = [int(row.strip()[-1]) for row in f.readlines()]

#################################################
# Prepare
#################################################
n = 10  # number of positions on board
n_end = 21  # number of points of end game

# Create dice transition matrix
row = np.eye(n, dtype=np.ulonglong)[0]
roll_mat = np.array([np.roll(row, i + 1) for i in range(n)], dtype=np.ulonglong)
roll_mat = roll_mat + roll_mat.dot(roll_mat) + roll_mat.dot(roll_mat).dot(roll_mat)
roll_mat = np.linalg.matrix_power(roll_mat, 3).T

# Create pawn move transition function
def move_pawn(pos: np.array) -> np.array:
    pos = pos.copy()
    for idx, row in enumerate(pos):
        temp = np.zeros(row.shape)
        temp[idx + 1:] = row[:-(idx + 1)]
        temp[-1] += row[-(idx + 1):].sum()
        pos[idx] = temp
    #
    return pos

# Prepare starting positions
pos = {0: np.zeros((n, n_end + 1), dtype=np.ulonglong), 1: np.zeros((n, n_end + 1), dtype=np.ulonglong)}
pos[0][data[0] - 1, 0] = 1
pos[1][data[1] - 1, 0] = 1

#################################################
# Process
#################################################
turn = 0
winning = np.zeros((1,2), dtype= np.ulonglong).flatten()
while all([p.sum() > 0 for p in pos.values()]):
    # Move pawn
    pos[turn] = move_pawn(roll_mat.dot(pos[turn]))
    # Check winnings
    winning[turn] += pos[turn][:, n_end].sum() * pos[1 - turn].sum()
    pos[turn][:, n_end] = 0
    # Go to next player
    turn = 1 - turn

#################################################
# Output
#################################################
print(f"{max(winning) = }")
