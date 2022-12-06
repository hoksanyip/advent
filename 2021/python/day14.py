from collections import Counter

#################################################
# Import
#################################################
with open("2021/data/day14.txt", "r") as f:
    polymer = list(f.readline().strip())
    template = Counter(a + b for a, b in zip(polymer[:-1], polymer[1:]))
    _ = f.readline()
    rules = [list(row.strip().replace(" -> ", "")) for row in f.readlines()]
    rules = {a + b: c for a, b, c in rules}

#################################################
# Process
#################################################
for i in range(40):
    new_template = Counter()
    for link, count in template.items():
        center = rules[link]
        new_template += Counter({link[0] + center: count})
        new_template += Counter({center + link[1]: count})
    template = new_template

counts = Counter({polymer[-1]: 1})
for link, count in template.items():
    counts += Counter({link[0]: count})

#################################################
# Output
#################################################
print(f"{max(counts.values()) - min(counts.values()) = }")
