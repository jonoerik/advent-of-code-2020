// To run, with type checking:
// node --run check && node --run run -- part1 data/input

import fs from 'fs/promises';
import path from 'path';
import yargs from 'yargs';
import { hideBin } from 'yargs/helpers';

const TileFloor = Symbol(".");
const TileEmpty = Symbol("L");
const TileFull = Symbol("#");

type TileType = typeof TileFloor | typeof TileEmpty | typeof TileFull;
type InputType = Array<Array<TileType>>;


async function load_input(path: string): Promise<InputType> {
    let result: InputType = [];
    const file = await fs.open(path);
    for await (const line of file.readLines()) {
        let result_line = []
        for (const c of line) {
            switch (c) {
                case '.':
                    result_line.push(TileFloor);
                    break;
                case 'L':
                    result_line.push(TileEmpty);
                    break;
                case '#':
                    result_line.push(TileFull);
                    break;
                default:
                    // Unexpected character in input.
                    console.assert(false);
                    break;
            }
        }
        result.push(result_line);
    }
    return result;
}


async function part1(input: InputType): Promise<number | undefined> {
    let chairs = input.map(r => r.map(i => i));
    function adjacent_full_seats(r: number, c: number): number {
        return [[-1, -1], [-1, 0], [-1, 1], [0, -1], [0, 1], [1, -1], [1, 0] ,[1, 1]].map(function(d: number[]): number {
            if (r + d[0] >= 0 && r + d[0] < chairs.length && c + d[1] >= 0 && c + d[1] < chairs[0].length) {
                return (chairs[r + d[0]][c + d[1]] == TileFull) ? 1 : 0;
            } else {
                return 0;
            }
        }).reduce((a, b) => a + b, 0);
    }

    function next_chairs(): InputType {
        return chairs.map((row, r) => row.map(function(item, c) {
            switch (item) {
                case TileFloor:
                    return TileFloor;
                    break;
                case TileEmpty:
                    return (adjacent_full_seats(r, c) == 0) ? TileFull : TileEmpty;
                    break;
                case TileFull:
                    return (adjacent_full_seats(r, c) >= 4) ? TileEmpty : TileFull;
                    break;
            }
        }));
    }
    let next = next_chairs();

    function chairs_changed(): boolean {
        for (let r = 0; r < chairs.length; ++r) {
            for (let c = 0; c < chairs[r].length; ++c) {
                if (chairs[r][c] !== next[r][c]) {
                    return true;
                }
            }
        }
        return false;
    }

    while (chairs_changed()) {
        chairs = next;
        next = next_chairs();
    }

    return chairs.map(row => row.map(function (item: TileType): number {return (item === TileFull) ? 1 : 0;}).reduce((a: number, b: number) => a + b)).reduce((a: number, b: number) => a + b);
}


async function part2(input: InputType): Promise<number | undefined> {
    //TODO
    return 0;
}


async function run_tests(): Promise<void> {
    async function get_int_from_file(path: string): Promise<number> {
        return parseInt(await fs.readFile(path, {"encoding": "utf8"}));
    }
    let all_passed = true;

    const answer1 = await get_int_from_file(path.join(".", "data", "sample1.answer1"));
    const computed1 = await part1(await load_input(path.join(".", "data", "sample1")));
    if (computed1 != answer1) {
        console.error("Test \'sample1\' part 1 failed: returned " + computed1 + ", expected " + answer1);
        all_passed = false;
    } else {
        console.log("Test \'sample1\' part 1 passed");
    }

    if (all_passed) {
        console.log("All tests passed");
    }
}


await yargs(hideBin(process.argv))
    .scriptName("")
    .usage("node --run run -- <command>")
    .command('part1 [path]', 'Run puzzle part 1', (yargs: yargs.Argv) => {
        yargs.positional('path', {
            type: 'string',
            describe: 'path to input data file'
        })
    }, async function (argv: any) {
        console.log(await part1(await load_input(argv.path)));
    })
    .command('part2 [path]', 'Run puzzle part 2', (yargs: yargs.Argv) => {
        yargs.positional('path', {
            type: 'string',
            describe: 'path to input data file'
        })
    }, async function (argv: any) {
        console.log(await part2(await load_input(argv.path)));
    })
    .command('test', 'Run unit tests', (yargs: yargs.Argv) => {}, async function (argv: any) {
        await run_tests();
    })
    .parse();
