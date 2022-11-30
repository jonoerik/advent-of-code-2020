#!/usr/bin/env python3

import argparse
import sys
from pathlib import Path

def load(input_path: Path) -> list[set[str]]:
    current_set = set()
    all_sets = []
    with open(input_path) as f:
        while True:
            line = f.readline()
            if not line or not line.strip():
                # EOF or blank line
                all_sets.append(current_set)
                current_set = set()
                if not line:
                    # EOF
                    break
            line = line.strip()
            current_set |= set([c for c in line])
    return all_sets


def part1(data: list[set[str]]) -> int:
    return sum([len(s) for s in data])


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("-1", "--part1", action="store_true")
    parser.add_argument("-2", "--part2", action="store_true")
    parser.add_argument("input")
    args = parser.parse_args()

    if args.part1 == args.part2:
        sys.exit("Exactly one of --part1 or --part2 must be specified.")

    data = load(Path(args.input))
    if args.part1:
        print(part1(data))
    else: #part2
        pass
