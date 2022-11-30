#!/usr/bin/env python3

import argparse
import sys
from pathlib import Path
import functools
import operator
import string

def load(input_path: Path) -> list[list[set[str]]]:
    current_group = []
    all_groups = []
    with open(input_path) as f:
        while True:
            line = f.readline()
            if not line or not line.strip():
                # EOF or blank line
                all_groups.append(current_group)
                current_group = []
                if not line:
                    # EOF
                    break
            line = line.strip()
            if line:
                current_group.append(set(line))
    return all_groups


def part1(data: list[list[set[str]]]) -> int:
    return sum([len(functools.reduce(operator.or_, group, set())) for group in data])


def part2(data: list[list[set[str]]]) -> int:
    return sum([len(functools.reduce(operator.and_, group, set(string.ascii_lowercase))) for group in data])


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
        print(part2(data))
