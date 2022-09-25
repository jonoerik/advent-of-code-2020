import std.stdio;
import std.getopt;
import std.exception;
import std.typecons;
import std.file;
import std.string;

enum Part {
    Part1,
    Part2,
}
struct Options {
    Part part;
    string input_path;
}

// Parse command-line options.
Options getOptions(string[] args) {
    bool part1 = false;
    bool part2 = false;
    auto opts = getopt(
        args,
        "part1|1", &part1,
        "part2|2", &part2,
    );

    Options result;
    if (part1 && part2) {
        throw new Exception("Only one of --part1 or --part2 may be specified.");
    } else if (part1) {
        result.part = Part.Part1;
    } else if (part2) {
        result.part = Part.Part2;
    } else {
        throw new Exception("Either --part1 or --part2 must be specified.");
    }

    if (args.length < 2) {
        throw new Exception("No input file path provided.");
    } else if (args.length > 2) {
        throw new Exception("Too many command line arguments provided.");
    } else {
        result.input_path = args[1];
    }

    return result;
}

// Load puzzle input from file, returning it as a 2D array of booleans,
// where true indicates a tree and false indicates an empty section.
bool[][] loadInput(string path) {
    auto f = File(path, "r");
    bool[][] result;
    while (!f.eof()) {
        string line = strip(f.readln());
        //TODO
    }
    return result;
}

// Distance travelled in each direction per step of the puzzle.
enum VELOCITY = Tuple!(uint, "right", uint, "down")(3, 1);

void main(string[] args) {
    auto opts = getOptions(args);
    auto input = loadInput(opts.input_path);
    //TODO
}
