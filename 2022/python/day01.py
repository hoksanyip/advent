from typing import Iterable


#################################################
# Import
#################################################
def process_deer(source: Iterable[str]) -> Iterable[int]:
    """
    Iterator to process the total food per deer.
    """
    deer = 0
    while line := source.readline():
        if line.strip() == "":
            yield deer
            deer = 0
        else:
            deer += int(line.strip())
    yield deer


with open("2022/src/main/resources/day01.txt", "r") as f:
    source = process_deer(f)
    data = list(source)


data.sort(reverse=True)

#################################################
# Output
#################################################
print(f"Answer 1: {data[0]}")

print(f"Answer 2: {sum(data[:3])}")
