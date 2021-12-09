import numpy as np

# Import
with open("src/main/resources/day04.txt", "r") as f:
    draws = [int(value) for value in f.readline().strip().split(",")]
    _ = f.readline()
    boards = "".join(f.readlines()).split("\n\n")

# Prepare
for idx, board in enumerate(boards):
    boards[idx] = np.array([
        [int(cel) for cel in row.strip().split(" ") if cel != ""]
        for row in board.split("\n")
    ])

# Process
t_bingo = np.zeros(len(boards), dtype=int)
t_bingo_mat = []
for idx, board in enumerate(boards):
    t = np.vectorize(draws.index)(board)
    t_bingo_mat.append(t)
    t_bingo[idx] = min(
        min(np.apply_along_axis(np.max, 1, t)),
        min(np.apply_along_axis(np.max, 0, t))
    )

idx = np.argmax(t_bingo)
winning_board = boards[idx]
winning_times = t_bingo_mat[idx]
t_end = t_bingo[idx]
score = sum(winning_board[winning_times > t_end]) * draws[t_end]

# Output
print(f"{score = }")
