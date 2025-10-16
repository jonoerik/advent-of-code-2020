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


def find_corner_tiles(input: Input) -> tuple[list[int], int]:
    """
    Return the IDs of the four corner tiles, and the number of times the first returned corner tile
    should be rotated clockwise 90 degrees so that it correctly sits in the top-left corner of the final image.
    """

    def norm_edge(edge: list[bool]) -> list[bool]:
        """An edge is normalised if it comes lexicographically before its reverse."""
        if edge < list(reversed(edge)):
            return edge
        else:
            return list(reversed(edge))

    def get_edges(tile: list[list[bool]]) -> list[list[bool]]:
        """Get the top, right, bottom, and left edges of a tile, as an ordered list."""
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

    tile_0_rotation = {
        (True, False, False, True): 0,
        (False, False, True, True): 1,
        (False, True, True, False): 2,
        (True, True, False, False): 3,
    }[tuple([edge in unique_edges for edge in tile_edges[corner_tiles[0]]])]

    return corner_tiles, tile_0_rotation


def part1(input: Input) -> int:
    return math.prod(find_corner_tiles(input)[0])


def part2(input: Input) -> int:
    def flip_v(tile: list[list[bool]]) -> list[list[bool]]:
        """Flip top-to-bottom."""
        return list(reversed(tile))

    def flip_h(tile: list[list[bool]]) -> list[list[bool]]:
        """Flip left-to-right."""
        return [list(reversed(row)) for row in tile]

    def transpose(tile: list[list[bool]]) -> list[list[bool]]:
        return list(map(list, zip(*tile)))

    def rotate(tile: list[list[bool]], times: int) -> list[list[bool]]:
        """times: number of times to rotate 90 degrees clockwise."""
        match times:
            case 0:
                return tile
            case 1:
                return flip_h(transpose(tile))
            case 2:
                return flip_v(flip_h(tile))
            case 3:
                return flip_v(transpose(tile))

    sea_monster = ["                  # ",
                   "#    ##    ##    ###",
                   " #  #  #  #  #  #   "]
    sea_monster = [[cell == "#" for cell in row] for row in sea_monster]

    corners, tile_0_rotation = find_corner_tiles(input)
    tiles_across = math.isqrt(len(input))

    # Find the left column of tiles first.
    matched_tiles = [rotate(input[corners[0]], tile_0_rotation)]
    remaining_tiles = [tile for tile_id, tile in input.items() if tile_id != corners[0]]

    def find_top_match(edge: list[bool]) -> list[list[bool]] | None:
        """Find a tile whose top edge matches edge.
        Returns the match, rotated and flipped as needed, if found in remaining_tiles, and removes
        it from remaining_tiles.
        Asserts if no match is found."""
        for tile in remaining_tiles:
            top = tile[0]
            right = [row[-1] for row in tile]
            bottom = tile[-1]
            left = [row[0] for row in tile]
            edge_reversed = list(reversed(edge))

            if top == edge:
                remaining_tiles.remove(tile)
                return tile
            elif top == edge_reversed:
                remaining_tiles.remove(tile)
                return flip_h(tile)
            elif right == edge:
                remaining_tiles.remove(tile)
                return rotate(tile, 3)
            elif right == edge_reversed:
                remaining_tiles.remove(tile)
                return rotate(flip_v(tile), 3)
            elif bottom == edge:
                remaining_tiles.remove(tile)
                return flip_v(tile)
            elif bottom == edge_reversed:
                remaining_tiles.remove(tile)
                return rotate(tile, 2)
            elif left == edge:
                remaining_tiles.remove(tile)
                return transpose(tile)
            elif left == edge_reversed:
                remaining_tiles.remove(tile)
                return rotate(tile, 1)
        assert False

    for _ in range(tiles_across - 1):
        matched_tiles.append(find_top_match(matched_tiles[-1][-1]))

    matched_tiles = [[tile] for tile in matched_tiles]
    for row in matched_tiles:
        for _ in range(tiles_across - 1):
            row.append(transpose(find_top_match([r[-1] for r in row[-1]])))

    assert len(remaining_tiles) == 0

    # Assemble the tiles.
    all_tiles = [[elem for tilecol in row for elem in tilecol[1:-1]] for tilerow in [list(map(list, zip(*tilerow))) for tilerow in matched_tiles] for row in tilerow[1:-1]]
    # Copy of all_tiles, from which we'll remove sea monster tiles. This allows us to account for potentially overlapping monsters.
    all_rough_seas = [[elem for elem in row] for row in all_tiles]

    for modified_monster in [mm for rm in [rotate(sea_monster, r) for r in range(4)] for mm in [rm, flip_v(rm)]]:
        # For each possible rotation/flip of the monster, search all possible positions.
        for dr in range(0, len(all_tiles) - len(modified_monster) + 1):
            for dc in range(0, len(all_tiles[0]) - len(modified_monster[0]) + 1):
                def monster_found() -> bool:
                    for mr, mrow in enumerate(modified_monster):
                        for mc, mcell in enumerate(mrow):
                            if mcell and not all_tiles[dr + mr][dc + mc]:
                                return False
                    return True
                if monster_found():
                    for mr, mrow in enumerate(modified_monster):
                        for mc, mcell in enumerate(mrow):
                            if mcell:
                                all_rough_seas[dr + mr][dc + mc] = False

    return sum([row.count(True) for row in all_rough_seas])


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
