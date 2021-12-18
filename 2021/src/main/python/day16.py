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
    #
    def take(self, i: int):
        msg = self.data[0:i]
        self.data = self.data[i:]
        return msg
    #
    def __len__(self):
        return len(self.data)


def parse(stream: Stream):
    version = int(stream.take(3), 2)
    type_id = int(stream.take(3), 2)
    # parse literal
    if type_id == 4:
        value = ""
        go = True
        while go:
            bit = stream.take(5)
            go = bit[0] == "1"
            value += bit[1:]
        return Package(version, type_id, None, int(value, 2), [])
    else:
        id = stream.take(1)
        # parse by size
        if id == "0":
            size = int(stream.take(15), 2)
            substream = Stream(stream.take(size))
            content = []
            while size > 0:
                el = parse(substream)
                size = len(substream)
                content.append(el)
            return Package(version, type_id, id, None, content)
        else:
            # Parse several times
            n = int(stream.take(11), 2)
            content = []
            for _ in range(n):
                content.append(parse(stream))
            return Package(version, type_id, id, None, content)


def calc_value(pkg: Package) -> int:
    if pkg.type_id == 0: # sum
        return sum(calc_value(kid) for kid in pkg.children)
    elif pkg.type_id == 1: # product
        return reduce(lambda x, y: x * y, [calc_value(kid) for kid in pkg.children])
    elif pkg.type_id == 2: # min
        return min(calc_value(kid) for kid in pkg.children)
    elif pkg.type_id == 3: # max
        return max(calc_value(kid) for kid in pkg.children)
    elif pkg.type_id == 4: # itself
        return pkg.value
    elif pkg.type_id == 5: # gt
        return 1 if calc_value(pkg.children[0]) > calc_value(pkg.children[1]) else 0
    elif pkg.type_id == 6: # lt
        return 1 if calc_value(pkg.children[0]) < calc_value(pkg.children[1]) else 0
    elif pkg.type_id == 7: # eq
        return 1 if calc_value(pkg.children[0]) == calc_value(pkg.children[1]) else 0


content = parse(Stream(data))

#################################################
# Process
#################################################

#################################################
# Output
#################################################
print(f"{calc_value(content) = }")
