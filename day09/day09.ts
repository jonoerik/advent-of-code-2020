// To run, with type checking:
// node --run check && node --run run -- part1 data/input

import fs from 'fs/promises';
import path from 'path';
import yargs from 'yargs';
import { hideBin } from 'yargs/helpers';


async function* load_input(path: string): AsyncIterable<number> {
    const file = await fs.open(path);
    for await (const line of file.readLines()) {
        yield parseInt(line);
    }
}


async function part1(input: AsyncIterable<number>, preamble_length: number = 25): Promise<number | undefined> {
    const previous: Array<number> = await Array.fromAsync({length: preamble_length}, await (async it => async () => (await it.next()).value)(input[Symbol.asyncIterator]()));
    function is_sum_of_previous(n: number): boolean {
        const seeking: Array<number> = [];
        for (const a of previous) {
            if (seeking.includes(a)) {
                return true;
            } else {
                seeking.push(n - a);
            }
        }
        return false;
    }

    for await (const next of input) {
        if (is_sum_of_previous(next)) {
            previous.shift();
            previous.push(next);
        } else {
            return next;
        }
    }

    // At least one number should have not been a sum of two of the previous n numbers.
    console.assert(false);
}


async function part2(input: AsyncIterable<number>, preamble_length: number = 25): Promise<number | undefined> {
    const previous: Array<number> = await Array.fromAsync({length: preamble_length}, await (async it => async () => (await it.next()).value)(input[Symbol.asyncIterator]()));
    const input_data: Array<number> = Array.from(previous);
    function is_sum_of_previous(n: number): boolean {
        const seeking: Array<number> = [];
        for (const a of previous) {
            if (seeking.includes(a)) {
                return true;
            } else {
                seeking.push(n - a);
            }
        }
        return false;
    }

    let target: number | null = null;
    for await (const next of input) {
        if (is_sum_of_previous(next)) {
            previous.shift();
            previous.push(next);
        } else {
            target = next;
            break;
        }
        input_data.push(next);
    }

    console.assert(target !== null);

    for (let end = input_data.length; end > 0; --end) {
        for (let start = end - 1; start >= 0; --start) {
            const slice = input_data.slice(start, end)
            if (slice.reduce((a, b) => a + b, 0) === target) {
                return slice.reduce((a, b) => Math.min(a, b)) + slice.reduce((a, b) => Math.max(a, b));
            }
        }
    }

    // A solution to the puzzle should have been found.
    console.assert(false);
}


async function run_tests(): Promise<void> {
    async function get_int_from_file(path: string): Promise<number> {
        return parseInt(await fs.readFile(path, {"encoding": "utf8"}));
    }
    let all_passed = true;

    const preamble = await get_int_from_file(path.join(".", "data", "sample1.preamble_length"));
    const answer1 = await get_int_from_file(path.join(".", "data", "sample1.answer1"));
    const computed1 = await part1(load_input(path.join(".", "data", "sample1")), preamble);
    if (computed1 != answer1) {
        console.error("Test \'sample1\' part 1 failed: returned " + computed1 + ", expected " + answer1);
        all_passed = false;
    } else {
        console.log("Test \'sample1\' part 1 passed");
    }

    const answer2 = await get_int_from_file(path.join(".", "data", "sample1.answer2"));
    const computed2 = await part2(load_input(path.join(".", "data", "sample1")), preamble);
    if (computed2 != answer2) {
        console.error("Test \'sample1\' part 2 failed: returned " + computed2 + ", expected " + answer2);
        all_passed = false;
    } else {
        console.log("Test \'sample1\' part 2 passed");
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
        console.log(await part1(load_input(argv.path)));
    })
    .command('part2 [path]', 'Run puzzle part 2', (yargs: yargs.Argv) => {
        yargs.positional('path', {
            type: 'string',
            describe: 'path to input data file'
        })
    }, async function (argv: any) {
        console.log(await part2(load_input(argv.path)));
    })
    .command('test', 'Run unit tests', (yargs: yargs.Argv) => {}, async function (argv: any) {
        await run_tests();
    })
    .parse();
