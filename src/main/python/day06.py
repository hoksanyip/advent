from collections import Counter

# Import
with open("data/day06.txt", "r") as f:
    data = Counter([int(v) for v in f.readline().split(",")])

# Prepare
n, m = 256, 7
pop = [data.get(i, 0) for i in range(m + 2)]

# Process
for i in range(n):
    pop = pop[1:] + pop[:1]
    pop[m - 1] += pop[m + 1]

# Output
print(f"{sum(pop) = }")
