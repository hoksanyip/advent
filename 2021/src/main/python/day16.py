from dataclasses import dataclass
from typing import List, Tuple
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


def parse(data):
    version = int(data[0:3], 2)
    type_id = int(data[3:6], 2)
    data = data[6:]
    # parse literal
    if type_id == 4:
        value = ""
        go = True
        while go:
            bit = data[:5]
            data = data[5:]
            go = bit[0] == "1"
            value += bit[1:]
        return Package(version, type_id, None, int(value, 2), []), data
    else:
        id = data[0]
        data = data[1:]
        # parse by size
        if id == "0":
            size = int(data[0:15], 2)
            data = data[15:]
            subdata = data[0:size]
            data = data[size:]
            content = []
            while size > 0:
                el, subdata = parse(subdata)
                size = len(subdata)
                content.append(el)
            return Package(version, type_id, id, None, content), data
        else:
            # Parse several times
            n = int(data[0:11], 2)
            data = data[11:]
            content = []
            for i in range(n):
                el, data = parse(data)
                content.append(el)
            return Package(version, type_id, id, None, content), data


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


content, buffer = parse(data)

#################################################
# Process
#################################################

#################################################
# Output
#################################################
print(f"{calc_value(content) = }")
