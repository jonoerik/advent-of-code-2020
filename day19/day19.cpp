#include <cstdlib>
#include <stdexcept>
#include <filesystem>
#include <fstream>
#include <iostream>
#include <queue>
#include <regex>
#include <sstream>
#include <stack>
#include <string>
#include <string_view>
#include <unordered_map>
#include <variant>
#include <vector>

#include <boost/algorithm/string.hpp>
#include <boost/any.hpp>
#include <boost/lexical_cast.hpp>
#include <boost/program_options/options_description.hpp>
#include <boost/program_options/parsers.hpp>
#include <boost/program_options/positional_options.hpp>
#include <boost/program_options/variables_map.hpp>

//! Exit status if --help was requested.
#define EXIT_HELP 2;


//! Either the index of another rule, or a literal string to be matched.
using rule_element_t = std::variant<size_t, std::string>;
//! A chain of rule elements, all of which must be matched consecutively.
using rule_chain_t = std::vector<rule_element_t>;
//! A set of rule chains, only one of which must be matched.
using rule_options_t = std::vector<rule_chain_t>;
//! A single top level rule.
using rule_t = rule_options_t;

class Input {
public:
    std::unordered_map<size_t, rule_t> rules;
    std::vector<std::string> messages;

    Input(std::unordered_map<size_t, rule_t> r, std::vector<std::string> m) :
        rules(std::move(r)),
        messages(std::move(m))
        {}
};

using input_t = Input;
using result_t = uint64_t;

enum class PartNum {
    Part1,
    Part2,
};


input_t load_input(std::filesystem::path path) {
    std::fstream f(path, std::fstream::in);
    if (!f.is_open()) {
        throw std::runtime_error("Input file could not be opened.");
    }

    // Leave this regex slightly more flexible than needed; allow rules like 5: "a" "b" | "c" 3
    std::regex rule_re(R"___(^(\d+):\s*((?:(?:"[[:alpha:]]"|\d+)(?:\s*(?:"[[:alpha:]]"|\d+))*)(?:\s*\|\s*(?:"[[:alpha:]]"|\d+)(?:\s*(?:"[[:alpha:]]"|\d+))*)*)$)___");
    std::unordered_map<size_t, rule_t> rules;
    while (!f.eof()) {
        std::string current_line;
        std::getline(f, current_line);
        if (current_line.size() == 0) {
            break;
        }

        std::smatch match;
        if (!std::regex_match(current_line, match, rule_re)) {
            throw std::runtime_error("Input rule line doesn't match regex.");
        }
        size_t rule_index = boost::lexical_cast<size_t>(match[1].str());

        std::vector<std::string> parts1;
        boost::split(parts1, match[2].str(), boost::is_any_of("|"));
        rule_options_t rule_options;
        for (auto& part1 : parts1) {
            std::vector<std::string> parts2;
            boost::split(parts2, boost::trim_copy(part1), boost::is_any_of(" "));
            rule_chain_t rule_chain;
            for (auto& part2 : parts2) {
                std::string part2_trimmed = boost::trim_copy(part2);
                rule_element_t rule_element;
                if (part2_trimmed.contains('\"')) {
                    std::erase(part2_trimmed, '\"');
                    rule_element = part2_trimmed;
                } else {
                    rule_element = boost::lexical_cast<size_t>(part2_trimmed);
                }
                rule_chain.push_back(rule_element);
            }
            rule_options.push_back(rule_chain);
        }
        rules[rule_index] = std::move(rule_options);
    }

    if (f.eof()) {
        throw std::runtime_error("Unexpected input file format.");
    }

    std::vector<std::string> messages;
    while (!f.eof()) {
        std::string current_line;
        std::getline(f, current_line);
        if (current_line.length() == 0) {
            break;
        }
        messages.push_back(std::move(current_line));
    }

    return Input{rules, messages};
}


// Needed for std::visit.
template<class... Ts>
struct visit_lambdas : Ts... { using Ts::operator()...; };

result_t part1(const input_t& input) {
    // Input rules describe a Context Free Language.
    // To test if each message is in the language, we convert
    // the language to a Pushdown Automaton (PDA):
    // - Input alphabet of letters, (implemented as std::string elements)
    // - Stack alphabet of letters and rule indices (implemented as rule_element_t)
    // - Accepting state requires both input and stack to be empty
    // Transitions are determined from input.rules on the fly.
    // We run this PDA to test if each message is accepted.

    struct PdaState {
        std::string input;
        std::stack<rule_element_t> stack;
        PdaState(const std::string& i, const std::stack<rule_element_t>& s) :
            input(i),
            stack(s)
            {}
    };

    auto message_in_language = [&input](const std::string& message) -> bool {
        // Using a queue for states means we're performing a breadth first search of the state space,
        // so even if recursive rules could keep adding new states to the queue, we'll still find a terminating parse if
        // one exists.
        std::queue<PdaState> states;
        states.push(PdaState(message, std::stack<rule_element_t>({0uz})));

        while (!states.empty()) {
            PdaState current = std::move(states.front());
            states.pop();

            if (current.stack.empty()) {
                if (current.input.empty()) {
                    // PDA successfully parses message.
                    return true;
                } else {
                    // Input remains after stack emptied; so this PDA state fails
                    // to parse message.
                    continue;
                }
            }

            rule_element_t rule = std::move(current.stack.top());
            current.stack.pop();
            std::visit(visit_lambdas{
                [&](std::string s){
                    // Rule "x"
                    if (current.input.starts_with(s)) {
                        current.input.erase(0, s.length());
                        states.push(std::move(current));
                    } else {
                        // This PDA state can't match the input.
                    }
                },
                [&](size_t i){
                    // Rule 1, 2, etc
                    for (auto& rule_option : input.rules.at(i)) {
                        PdaState new_state(current.input, current.stack);
                        for (auto it = rule_option.rbegin(); it != rule_option.rend(); ++it) {
                            new_state.stack.push(*it);
                        }
                        states.push(std::move(new_state));
                    }
                }
            }, rule);
        }
        return false;
    };

    result_t result = 0;
    for (auto& message : input.messages) {
        if (message_in_language(message)) {
            ++result;
        }
    }
    return result;
}


result_t part2(const input_t& input) {
    input_t modified_input = input;
    modified_input.rules[8uz] = {{42uz}, {42uz, 8uz}};
    modified_input.rules[11uz] = {{42uz, 31uz}, {42uz, 11uz, 31uz}};
    return part1(modified_input);
}


std::unordered_map<PartNum, result_t (*)(const input_t&)> part_fns{
    {PartNum::Part1, &part1},
    {PartNum::Part2, &part2},
};


//! Run a single test at path \param test_input.
//! \return true if the test passed, false otherwise.
bool run_test(std::filesystem::path test_input_path, std::filesystem::path test_answer, PartNum part_num) {
    input_t input = load_input(test_input_path);
    result_t answer = part_fns[part_num](input);

    result_t expected = [&test_answer](){
        std::fstream f(test_answer, std::fstream::in);
        if (!f.is_open()) {
            throw std::runtime_error("Test answer file could not be opened.");
        }
        result_t result;
        f >> result;
        return result;
    }();

    return answer == expected;
}


//! Run all tests.
//! \return true if all tests pass, false otherwise.
bool run_tests() {
    bool all_passed = true;
    std::regex sample_regex("^sample[0-9]+$");
    for (auto const& dir_entry : std::filesystem::directory_iterator("../data")) {
        std::smatch match_results;
        std::string filename = dir_entry.path().filename().string();
        if (std::regex_match(filename, match_results, sample_regex)) {
            if (std::filesystem::exists(dir_entry.path().string() + ".answer1")) {
                // Technically suceptible to TOCTOU issues, but in test code for AoC we're probably alright to ignore that.
                if (run_test(dir_entry.path(), dir_entry.path().string() + ".answer1", PartNum::Part1)) {
                    std::cerr << filename << ": PART1: PASSED" << std::endl;
                } else {
                    std::cerr << filename << ": PART1: FAILED" << std::endl;
                    all_passed = false;
                }
            }
            if (std::filesystem::exists(dir_entry.path().string() + ".answer2")) {
                if (run_test(dir_entry.path(), dir_entry.path().string() + ".answer2", PartNum::Part2)) {
                    std::cerr << filename << ": PART2: PASSED" << std::endl;
                } else {
                    std::cerr << filename << ": PART2: FAILED" << std::endl;
                    all_passed = false;
                }
            }
        }
    }
    return all_passed;
}


int main(int argc, char* argv[]) {
    boost::program_options::options_description opts_desc("Allowed options");
    bool opts_part1;
    bool opts_part2;
    opts_desc.add_options()
        ("help", "Print help message")
        ("test", "Run tests")
        ("part1,1", boost::program_options::bool_switch(&opts_part1))
        ("part2,2", boost::program_options::bool_switch(&opts_part2))
        ("input_filename", boost::program_options::value<std::string>(), "Puzzle input file");
    boost::program_options::positional_options_description posopts_desc;
    posopts_desc.add("input_filename", 1);
    boost::program_options::variables_map vm;
    boost::program_options::store(boost::program_options::command_line_parser(argc, argv).options(opts_desc).positional(posopts_desc).run(), vm);
    boost::program_options::notify(vm);

    if (vm.contains("help")) {
        std::cerr << "Usage:\n\t" << argv[0] << " [options] <input_filename>\n\n";
        std::cerr << opts_desc << std::endl;
        return EXIT_HELP;
    }

    if (vm.contains("test")) {
        bool result = run_tests();
        return result ? EXIT_SUCCESS : EXIT_FAILURE;
    }

    auto filename_iter = vm.find("input_filename");
    if (filename_iter == vm.end()) {
        std::cerr << "No input filename specified." << std::endl;
        return EXIT_FAILURE;
    }
    std::string filename = any_cast<std::string>(filename_iter->second.value());

    PartNum part;
    if (opts_part1 && opts_part2) {
        std::cerr << "Can't specify both --part1 and --part2." << std::endl;
        return EXIT_FAILURE;
    } else if (opts_part1) {
        part = PartNum::Part1;
    } else if (opts_part2) {
        part = PartNum::Part2;
    } else {
        std::cerr << "Either --part1 or --part2 must be specified." << std::endl;
        return EXIT_FAILURE;
    }

    input_t input = load_input(filename);
    result_t result = part_fns[part](input);
    std::cout << result << std::endl;

    return EXIT_SUCCESS;
}
