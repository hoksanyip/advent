from typing import Dict, List, Set
from collections import Counter

#################################################
# Import
#################################################
with open("2021/data/day12.txt", "r") as f:
    data = [row.strip() for row in f.readlines()]
    data = [row.split("-") for row in data]

#################################################
# Prepare
#################################################
edges = set([edge for row in data for edge in row])
graph = {edge: set() for edge in edges}  # Dict[str, Set[str]]
for node in data:
    node_a, node_b = node
    graph[node_a].add(node_b)
    graph[node_b].add(node_a)

#################################################
# Process
#################################################


def disallow_caves(route: List[str]) -> bool:
    frequencies = Counter([cave for cave in route if cave.islower()])
    if frequencies.most_common()[0][1] > 1:
        # Only allow caves if it hasn't been visited before
        return set(frequencies.keys())
    else:
        # Allow an extra visit for 1 small cave
        return set()


def find_route(route: List[str], graph: Dict[str, Set[str]]) -> int:
    last_cave = route[-1]
    # End of cave
    if last_cave == "end":
        return 1

    # Find possible caves to navigate to
    options = graph[last_cave] - set(["start"]) - disallow_caves(route)

    # Try all options
    num_routes = 0
    for next_cave in options:
        new_route = route + [next_cave]
        num_routes += find_route(new_route, graph)

    return num_routes


route_count = find_route(["start"], graph)

#################################################
# Output
#################################################
print(f"{route_count = }")
