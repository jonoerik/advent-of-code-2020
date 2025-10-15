#!/usr/bin/env python3

import argparse
import math
from pathlib import Path
import sys


Input = dict[int, list[list[bool]]]


def load(input_path: Path) -> Input:
    with open(input_path) as f:
        return {
            int(tile[0].split(" ")[1][:-1]): [
                [cell == "#" for cell in row]
                for row in tile[1:]
            ]
            for tile in [
                tile.split("\n")
                for tile in f.read().strip().split("\n\n")
            ]
        }


def part1(input: Input) -> int:
    def norm_edge(edge: list[bool]) -> list[bool]:
        # An edge is normalised if it comes lexicographically before its reverse.
        if edge < list(reversed(edge)):
            return edge
        else:
            return list(reversed(edge))

    def get_edges(tile: list[list[bool]]) -> list[list[bool]]:
        # Get the top, right, bottom, and left edges of a tile, as an ordered list.
        return [norm_edge(edge) for edge in [
            tile[0],
            [row[-1] for row in tile],
            tile[-1],
            [row[0] for row in tile],
        ]]

    # {tile id: [tile edges]}
    tile_edges = {tile_id: get_edges(tile) for tile_id, tile in input.items()}
    # [all edges of all tiles]
    all_edges = [edge for edges in tile_edges.values() for edge in edges]
    # [all edges that only appear once]
    unique_edges = [edge for edge in all_edges if all_edges.count(edge) == 1]
    # [tile ids of all tiles that must lie on a corner]
    corner_tiles = [tile_id for tile_id, tile_edges in tile_edges.items()
                    if [tile_edge in unique_edges for tile_edge in tile_edges].count(True) == 2]

    # Fortunately, for both the given sample, and the actual input, all edges are unique except where they must match up.
    # No duplicate edges appear on the outermost edges of the image, and no matched tile edges are identical to any edges
    # except each other. This ensures that a tile meets one of the following 3 cases:
    #     4 non-unique edges: tile is completely surrounded by other tiles.
    #     3 non-unique edges: tile has one edge on the border of the overall image.
    #     2 unique and 2 non-unique edges: tile is on a corner.
    # If this didn't hold for the input, we'd have to search the possible arrangements of tiles to find the one that allows
    # a complete arrangement.
    assert len(corner_tiles) == 4
    return math.prod(corner_tiles)


def part2(input: Input) -> int:
    pass  # TODO


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
