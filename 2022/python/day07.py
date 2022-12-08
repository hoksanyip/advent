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
            case "$", "ls":  # Do nothing, last 2 cases will take care of it
                pass
            case "$", "cd", "..":  # Exit path
                cwd.pop()
            case "$", "cd", path:  # Enter folder
                cwd.append(path)
                content["/".join(cwd)] = []
            case "dir", folder:
                full_path = "/".join(cwd)
                content[full_path].append(Folder(f"{full_path}/{folder}"))
            case size, filename:
                full_path = "/".join(cwd)
                file_path = f"{full_path}/{filename}"
                content[full_path].append(File(file_path, int(size)))
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


THRESHOLD_SIZE = 100000
TOTAL_SIZE = 70000000
REQUIRED_SIZE = 30000000


sizes = [get_size(path, data) for path in data.keys()]
output1 = sum(size for size in sizes if size <= THRESHOLD_SIZE)

used = get_size("/", data)
seeking_size = TOTAL_SIZE - used - REQUIRED_SIZE
output2 = min(size for size in sizes if size >= seeking_size)

#################################################
# Output
#################################################
print(f"Answer 1: {output1}")
print(f"Answer 2: {output2}")
