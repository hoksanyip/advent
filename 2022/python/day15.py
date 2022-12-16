from pathlib import Path
from typing import Tuple, List
from utils import RegexMatch
from collections import defaultdict

#################################################
# Prepare
#################################################
Pos = Tuple[int, int]


def parse(lines: List[str]):
    sensors = dict()
    beacons = defaultdict(set)
    for line in lines:
        match RegexMatch(line):
            case "Sensor at x=(.*), y=(.*): closest beacon is at x=(.*), y=(.*)" as matches:
                xs, ys, xb, yb = map(int, matches.match.groups())
                distance = abs(xs - xb) + abs(ys - yb)
                sensors[(xs, ys)] = distance
                beacons[(xb, yb)].add((xs, ys))
    return sensors, beacons


#################################################
# Import
#################################################
data = Path("2022/data/day15.txt").read_text().split("\n")
sensors, beacons = parse(data)


#################################################
# Process
#################################################
BORDER_INDEX = 4_000_000


def has_overlap(set1: Tuple[int, int], set2: Tuple[int, int]) -> bool:
    return (
        (set1[0] - set2[0]) * (set2[1] - set1[1]) >= 0 or
        (set1[1] - set2[0]) * (set2[1] - set1[0]) >= 0
    )


def scan_row(row):
    scanned = set()
    for sensor, distance in sensors.items():
        xrange = distance - abs(row - sensor[1])
        if xrange > 0:
            new_set = (sensor[0] - xrange, sensor[0] + xrange)
            for known in scanned:
                if has_overlap(known, new_set):
                    scanned = scanned.difference({known})
                    new_set = (min(new_set[0], known[0]),
                               max(new_set[1], known[1]))
            scanned.add(new_set)
    return scanned


scanned = scan_row(BORDER_INDEX // 2)
impossible = sum(rng[1] - rng[0] + 1 for rng in scanned)
in_line = sum(beacon[1] == BORDER_INDEX // 2 for beacon in beacons.keys())
output1 = impossible - in_line


################################
# BRUTE FORCE
################################
row = next(i for i in range(BORDER_INDEX) if len(scan_row(i)) > 1)
scanned = scan_row(row)
col = next(
    j for j in range(BORDER_INDEX)
    if not any(has_overlap((j, j), scanner) for scanner in scanned)
)
output2 = col * BORDER_INDEX + row


#################################################
# Output
#################################################
print(f"Answer 1: {output1}")
print(f"Answer 2: {output2}")
