#include <cstdlib>
#include <stdexcept>
#include <filesystem>
#include <fstream>
#include <iostream>
#include <regex>
#include <string>
#include <sstream>
#include <unordered_map>
#include <vector>

#include <boost/any.hpp>
#include <boost/program_options/options_description.hpp>
#include <boost/program_options/parsers.hpp>
#include <boost/program_options/positional_options.hpp>
#include <boost/program_options/value_semantic.hpp>
#include <boost/program_options/variables_map.hpp>

//! Exit status if --help was requested.
#define EXIT_HELP 2;


//! Fields that must be present in a passport entry for it to be considered valid.
static const std::vector<const char*> REQUIRED_FIELDS = {
    "byr",
    "iyr",
    "eyr",
    "hgt",
    "hcl",
    "ecl",
    "pid",
    //"cid",
};

using ValidatorFn = bool (*)(const std::string& data);
static const std::unordered_map<const char*, ValidatorFn> VALIDATORS = {
    {"byr", [](const std::string& data){ return (std::regex_match(data, std::regex("^[0-9]{4}$"))) && (std::stoi(data) >= 1920) && (std::stoi(data) <= 2002); }},
    {"iyr", [](const std::string& data){ return (std::regex_match(data, std::regex("^[0-9]{4}$"))) && (std::stoi(data) >= 2010) && (std::stoi(data) <= 2020); }},
    {"eyr", [](const std::string& data){ return (std::regex_match(data, std::regex("^[0-9]{4}$"))) && (std::stoi(data) >= 2020) && (std::stoi(data) <= 2030); }},
    {"hgt", [](const std::string& data){
        if (std::regex_match(data, std::regex("^[0-9]+(cm|in)$"))) {
            int v = std::stoi(data.substr(0, data.length() - 2));
            if (data.ends_with("cm")) {
                return (v >= 150) && (v <= 193);
            } else if (data.ends_with("in")) {
                return (v >= 59) && (v <= 76);
            }
        }
        return false;
    }},
    {"hcl", [](const std::string& data){ return std::regex_match(data, std::regex("^#[0-9a-f]{6}$")); }},
    {"ecl", [](const std::string& data){ return std::regex_match(data, std::regex("^amb|blu|brn|gry|grn|hzl|oth$")); }},
    {"pid", [](const std::string& data){ return std::regex_match(data, std::regex("^[0-9]{9}$")); }},
    //{"cid", },
};

//! Check if a single passport entry contains all required fields.
//! \return true if the passport entry is valid, false otherwise.
//! \param validate_data Should the contents of each data field also be validated (puzzle part 2).
bool check_entry_valid(const std::vector<std::string>& data, bool validate_data) {
    for (const char* field : REQUIRED_FIELDS) {
        bool field_found = false;
        for (const std::string& data_field : data) {
            if (data_field.starts_with(field)) {
                field_found = true;
                if (validate_data) {
                    std::string field_contents = data_field.substr(data_field.find(':') + 1);
                    if (!VALIDATORS.at(field)(field_contents)) {
                        return false;
                    }
                }
                break;
            }
        }
        if (!field_found) {
            return false;
        }
    }
    return true;
}


//! \return The number of valid passport entries in the file \param filename.
//! \param validate_data Should the contents of each data field also be validated (puzzle part 2).
size_t count_valid_entries(std::filesystem::path filename, bool validate_data) {
    size_t valid_count = 0;
    std::fstream f(filename, std::fstream::in);
    if (!f.is_open()) {
        throw std::runtime_error("Input file could not be opened.");
    }

    // Entries for current passport.
    std::vector<std::string> entries;
    while (!f.eof()) {
        std::string current_line;
        std::getline(f, current_line);
        if (current_line.size() == 0) {
            valid_count += check_entry_valid(entries, validate_data) ? 1 : 0;
            entries.clear();
        } else {
            std::stringstream ss(current_line);
            std::string current_field;
            while (true) {
                current_field.clear();
                std::getline(ss, current_field, ' ');
                if (current_field.size() == 0) {
                    break;
                }
                entries.push_back(current_field);
            }
        }
    }

    valid_count += check_entry_valid(entries, validate_data) ? 1 : 0;

    return valid_count;
}


//! Run a single test at path \param test_input.
//! \return true if the test passed, false otherwise.
//! \param test_answer Path to file containing expected answer of test.
//! \param validate_data Should the contents of each data field also be validated (puzzle part 2).
bool run_test(std::filesystem::path test_input, std::filesystem::path test_answer, bool validate_data) {
    size_t valid_entries = count_valid_entries(test_input, validate_data);

    size_t expected_valid_entries = [&test_answer](){
        std::fstream f(test_answer, std::fstream::in);
        if (!f.is_open()) {
            throw std::runtime_error("Test answer file could not be opened.");
        }
        size_t result;
        f >> result;
        return result;
    }();

    return valid_entries == expected_valid_entries;
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
                if (run_test(dir_entry.path(), dir_entry.path().string() + ".answer1", false)) {
                    std::cerr << filename << ": PART1: PASSED" << std::endl;
                } else {
                    std::cerr << filename << ": PART1: FAILED" << std::endl;
                    all_passed = false;
                }
            }
            if (std::filesystem::exists(dir_entry.path().string() + ".answer2")) {
                if (run_test(dir_entry.path(), dir_entry.path().string() + ".answer2", true)) {
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

    bool part2 = false;
    if (opts_part1 && opts_part2) {
        std::cerr << "Can't specify both --part1 and --part2." << std::endl;
        return EXIT_FAILURE;
    } else if (opts_part1) {
        part2 = false;
    } else if (opts_part2) {
        part2 = true;
    } else {
        std::cerr << "Either --part1 or --part2 must be specified." << std::endl;
        return EXIT_FAILURE;
    }
    std::cout << count_valid_entries(filename, part2) << std::endl;

    return EXIT_SUCCESS;
}
