import numpy as np
from pathlib import Path
from utils import RegexMatch
from typing import Dict, List, Set
from dataclasses import dataclass
import heapq


#################################################
# Prepare
#################################################
@dataclass
class Valve:
    rate: int
    to: List[str]


def parse(lines: List[str]) -> Dict[str, Valve]:
    passage = dict()
    for line in lines:
        match RegexMatch(line):
            case "Valve (.*) has flow rate=(.*); tunnels? leads? to valves? (.*)" as _specs:
                source, rate, options = _specs.match.groups()
                passage[source] = Valve(int(rate), options.split(", "))
    return passage


#################################################
# Import
#################################################
data = Path("2022/data/day16.txt").read_text().split("\n")
passage = parse(data)


#################################################
# Process
#################################################
def fastest_path(passage: Dict[str, Valve], source: str, target: str):
    seen = []
    queue = [(0, float('nan'), [source])]
    while True:
        # Take best so far
        time, _, path = heapq.heappop(queue)
        current = path[-1]
        seen.append(current)
        if current == target:
            break
        # Go to next valves
        for option in passage[current].to:
            if option not in seen:
                heapq.heappush(
                    queue, (time + 1, float('nan'), path + [option]))
    return time, path


def search(passage: Dict[str, Valve], routes: Dict[str, Dict[str, int]], pressure: int = 0, path: List[str] = ["AA"], remaining: int = 30):
    # If beyond reach, stop
    if remaining < 0:
        return pressure, path[-1], [(path, pressure)]
    # open and update valve
    current = path[-1]
    pressure = pressure + passage[current].rate * remaining
    # Go to next valves
    #
    best_pressure = pressure
    best_path = path
    options = [(path, pressure)]
    for option, dt in routes[current].items():
        if option not in path and dt <= remaining:
            new_pressure, new_path, new_options = search(
                passage, routes, pressure, path + [option], remaining - dt - 1)
            options = options + new_options
            if new_pressure > best_pressure:
                best_pressure = new_pressure
                best_path = new_path
    #
    return best_pressure, best_path, options


targets = {
    key for key, valve in passage.items()
    if valve.rate != 0 or key == 'AA'
}

routes = {
    source: {
        target: fastest_path(passage, source, target)[0]
        for target in targets
    }
    for source in targets
}
best_pressure, best_path, options = search(passage, routes, 0, remaining=30)
output1 = best_pressure


best_pressure, best_path, options = search(passage, routes, 0, remaining=26)
options = [(set(o[0][1:]), o[1]) for o in options[1:]]
options.sort(key=lambda x: -x[1])

best = 0
remaining_options = options
for i, (setA, pressureA) in enumerate(options):
    if best > pressureA * 2:
        break
    remaining_options = remaining_options[1:]
    for setB, pressureB in remaining_options:
        if not setA & setB:
            bestA = pressureB + pressureA
            if bestA > best:
                best = bestA

output2 = best


#################################################
# Output
#################################################
print(f"Answer 1 : {output1}")
print(f"Answer 2 : {output2}")
