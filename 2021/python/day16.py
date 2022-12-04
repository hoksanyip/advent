from dataclasses import dataclass
from functools import reduce

#################################################
# Import
#################################################
with open("2021/src/main/resources/day16.txt", "r") as f:
    data = f.readline().strip()
    data = "".join(["{:0>4b}".format(int(h, 16)) for h in list(data)])

#################################################
# Prepare
#################################################
@dataclass
class Package():
    version: int
    type_id: int
    id: int
    value: int
    children: list

@dataclass
class Stream():
    data: str
    def __init__(self, data: str):
        self.data = data

    def take(self, i: int):
        msg = self.data[0:i]
        self.data = self.data[i:]
        return msg
    
    def peek(self, i: int):
        if len(self.data) < i:
            return self.data
        else:
            return self.data[0:i]

    def __len__(self):
        return len(self.data)


def parse(stream: Stream):
    version = int(stream.take(3), 2)
    type_id = int(stream.take(3), 2)
    id = stream.peek(1)
    if type_id == 4:
        # parse literal
        value = ""
        bit = "1"
        while bit[0] == "1":
            bit = stream.take(5)
            value += bit[1:]
        return Package(version, type_id, None, int(value, 2), [])
    elif id == "0":
        # parse by size
        _ = stream.take(1)
        size = int(stream.take(15), 2)
        substream = Stream(stream.take(size))
        content = []
        while len(substream) > 0:
            content.append(parse(substream))
        return Package(version, type_id, id, None, content)
    else:
        # Parse several times
        _ = stream.take(1)
        n = int(stream.take(11), 2)
        content = []
        for _ in range(n):
            content.append(parse(stream))
        return Package(version, type_id, id, None, content)


def calc_value(pkg: Package) -> int:
    fun = {
        0: lambda x, y: x + y,
        1: lambda x, y: x * y,
        2: min,
        3: max,
        5: lambda x, y: 1 if x > y else 0,
        6: lambda x, y: 1 if x < y else 0,
        7: lambda x, y: 1 if x == y else 0
    }
    if pkg.type_id == 4:
        return pkg.value
    else:
        return reduce(fun[pkg.type_id], [calc_value(kid) for kid in pkg.children])

#################################################
# Process
#################################################
content = parse(Stream(data))

#################################################
# Output
#################################################
print(f"{calc_value(content) = }")
