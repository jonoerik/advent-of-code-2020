#!/usr/bin/env python3

import argparse
import sys
from pathlib import Path

# Dictionary of `bag colour` to `list of bag types contained within`.
# Where each contained bag is a tuple of (contained bag colour, count of bags of that colour).
BagDict = dict[str, list[tuple[str, int]]]


def load(input_path: Path) -> BagDict:
    bags = {}
    with open(input_path) as f:
        while True:
            line = f.readline()
            if not line:
                break
            line = line.strip()
            (this_bag, remainder) = line.split(" bags contain ")
            remainder = remainder[:-1]  # Remove trailing '.'
            if remainder == "no other bags":
                bags[this_bag] = []
            else:
                contained_bags = []
                for contained_bags_str in remainder.split(", "):
                    parts = contained_bags_str.split(" ")
                    contained_bags.append((" ".join(parts[1:3]), int(parts[0])))
                bags[this_bag] = contained_bags
    return bags


def count_contained(bags: BagDict, outer_colour: str, count_colour: str) -> int:
    """
    For a bag-contents dictionary of `bags`, and an outermost bag `outer_colour`,
    how many bags of `count_colour` are contained within (potentially including
    the outermost one).
    """
    count = 1 if outer_colour == count_colour else 0
    for next_bag, next_bag_count in bags[outer_colour]:
        count += next_bag_count * count_contained(bags, next_bag, count_colour)
    return count


def part1(bags: BagDict) -> int:
    return sum([1 if count_contained(bags, colour, "shiny gold") > 0 else 0 for colour in bags.keys() if colour != "shiny gold"])


def part2(bags: BagDict) -> int:
    pass #TODO


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
    else:  # part2
        print(part2(data))
