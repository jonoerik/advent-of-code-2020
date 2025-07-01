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


async function part2(input: AsyncIterable<number>): Promise<number> {
    //TODO
    return 0;
}


async function run_tests(): Promise<void> {
    async function get_int_from_file(path: string): Promise<number> {
        return parseInt(await fs.readFile(path, {"encoding": "utf8"}));
    }
    async function run_test(name: string): Promise<boolean> {
        const preamble = await get_int_from_file(path.join(".", "data", name + ".preamble_length"));
        const answer = await get_int_from_file(path.join(".", "data", name + ".answer1"));
        const computed = await part1(load_input(path.join(".", "data", name)), preamble);
        if (computed != answer) {
            console.error("Test \'" + name + "\' failed: returned " + computed + ", expected " + answer);
            return false;
        } else {
            console.log("Test \'" + name + "\' passed");
            return true;
        }
    }
    if (! await run_test("sample1")) { return; }
    console.log("All tests passed")
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
