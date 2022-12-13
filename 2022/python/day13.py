from pathlib import Path


#################################################
# Prepare
#################################################
def cmp(a: int | list, b: int | list) -> int:
    if type(a) is int and type(b) is int:
        # If both are integers
        return (a > b) - (b > a)
    elif type(a) is int:
        # if left is integer
        return cmp([a], b)
    elif type(b) is int:
        # if right is integer
        return cmp(a, [b])
    elif len(a) == 0 and len(b) == 0:
        # If both are empty
        return 0
    elif len(a) == 0:
        # If left is empty
        return -1
    elif len(b) == 0:
        # If right is empty
        return 1

    head_compare = cmp(a[0], b[0])
    if head_compare != 0:
        return head_compare
    else:
        return cmp(a[1:], b[1:])


#################################################
# Import
#################################################
data = [
    [eval(content) for content in pair.split("\n")]
    for pair in Path("2022/data/day13.txt").read_text().split("\n\n")
]


#################################################
# Process
#################################################
DIVIDER_PACKAGES = ([[2]], [[6]])

output1 = sum(
    i + 1 for i, pair in enumerate(data)
    if cmp(pair[0], pair[1]) < 0
)

data = [content for pair in data for content in pair]
pos1 = len([
    content for content in data
    if cmp(content, DIVIDER_PACKAGES[0]) < 0
]) + 1
pos2 = len([
    content for content in data
    if cmp(content, DIVIDER_PACKAGES[1]) < 0
]) + 2
output2 = pos1 * pos2

#################################################
# Output
#################################################
print(f"Answer 1: {output1}")
print(f"Answer 2: {output2}")
