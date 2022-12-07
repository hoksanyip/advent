import io
from dataclasses import dataclass
from __future__ import annotations


#################################################
# Prepare
#################################################
@dataclass
class Path():
    name: str


@dataclass
class Folder(Path):
    pass


@dataclass
class File(Path):
    size: int


def parse_browser(source: io.TextIOWrapper):
    cwd = []
    content = {}
    while line := source.readline():
        match line.replace("\n", "").split(" "):
            case "$", "cd", "..":
                # Exit path
                cwd.pop()
            case "$", "cd", path:
                # Enter folder
                cwd.append(path)
                content["/".join(cwd)] = []
            case "$", "ls":
                # Do nothing, next case will take care of it
                pass
            case "dir", folder:
                full_path = "/".join(cwd)
                content[full_path].append(Folder(f"{full_path}/{folder}"))
            case size, filename:
                full_path = "/".join(cwd)
                content[full_path].append(
                    File(f"{full_path}/{filename}", int(size)))
    return content


#################################################
# Import
#################################################
with open("2022/data/day07.txt", "r") as f:
    data = parse_browser(f)


#################################################
# Process
#################################################
def get_size(path: Path, data: dict):
    size = 0
    for content in data[path]:
        match content:
            case File(_, file_size):
                size += file_size
            case Folder(name):
                size += get_size(name, data)
    return size


sizes = [get_size(path, data) for path in data.keys()]
output1 = sum(size for size in sizes if size <= 100000)


TOTAL_SIZE = 70000000
REQUIRED_SIZE = 30000000
used = get_size("/", data)

output2 = min(size for size in sizes if TOTAL_SIZE - used + size >= REQUIRED_SIZE)

#################################################
# Output
#################################################
print(f"Answer 1: {output1}")
print(f"Answer 2: {output2}")
