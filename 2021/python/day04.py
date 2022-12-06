import numpy as np

#################################################
# Import
#################################################
with open("2021/data/day04.txt", "r") as f:
    draws = [int(value) for value in f.readline().strip().split(",")]
    _ = f.readline()
    boards = "".join(f.readlines()).split("\n\n")

#################################################
# Prepare
#################################################
for idx, board in enumerate(boards):
    board = [row.strip().split(" ") for row in board.split("\n")]
    board = [[int(cell) for cell in row if cell != ""] for row in board]
    boards[idx] = np.array(board)

#################################################
# Process
#################################################
t_bingo = np.zeros(len(boards), dtype=int)
t_bingo_mat = []
for idx, board in enumerate(boards):
    # Find time of draw per element on board
    t = np.vectorize(draws.index)(board)
    t_bingo_mat.append(t)
    # Find first t of bingo
    row_min = min(np.apply_along_axis(np.max, 1, t))
    col_min = min(np.apply_along_axis(np.max, 0, t))
    t_bingo[idx] = min(row_min, col_min)

idx = np.argmax(t_bingo)
winning_board = boards[idx]
winning_times = t_bingo_mat[idx]
t_end = t_bingo[idx]
score = sum(winning_board[winning_times > t_end]) * draws[t_end]

#################################################
# Output
#################################################
print(f"{score = }")
