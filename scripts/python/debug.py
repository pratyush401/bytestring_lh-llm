from parse_files import *
from pprint import pprint

def test_regex_patterns():
    pattern0 = r"(?:\n)(.+)(?:\n?\s*::)"
    pattern1 = r"(?:\n)(.+?)(?:\s*)(?:\n?\s*::)"
    pattern2 = r"(?:\n)([^\s]+\s?[^\s]+)(?:\n?\s*::)"
    pattern3 = r"({-@ [^\s]+?)(?:\n?\s*::)"                 # PATTERN_LH_FUNCTIONS in parse_files.py
    pattern4 = r"(?:(?:\n)|(?:{-@ ))([^\s-]+)(?:\n?\s*::)"
    pattern5 = r"(?:\n)([^\s-]+)(?:\n?\s*::)"               # does not allow spaces before function name
    pattern6 = r"(?:\n\s*)([^\s-]+)(?:\n?\s*::)"            # PATTERN_ALL_FUNCTIONS in parse_files.py

    result1 = get_func_source_files(mode="write", pattern=pattern5, key="function", fname="scripts/outputs/result1.csv")
    result2 = get_func_source_files(mode="write", pattern=pattern6, key="function", fname="scripts/outputs/result2.csv")
    set1 = set(map(lambda s: s.strip().replace("{-@ ", ""), result1))
    set2 = set(map(lambda s: s.strip(), result2))

    print(f"\nItems in set1 : {len(set1)}\nItems in set2 : {len(set2)}")

    diff1_2 = set1.difference(set2)
    print("\nNumber of functions in set1 but not set2 :", len(diff1_2))
    pprint(diff1_2)

    diff2_1 = set2.difference(set1)
    print("\nNumber of functions in set2 but not set1 :", len(diff2_1))
    pprint(diff2_1)

    intersect = set1.intersection(set2)
    print("\nNumber of functions shared by set1 and set2 :", len(intersect))
    # pprint(intersect)


def test_get_function_contents():
    function_source_files = read_csv_to_dict("scripts/inputs/function_source_files.csv")
    pprint(get_function_contents("word64BE", file_name=function_source_files["word64BE"][1])[0][1])


def test_print_lh_functions():
    lh_functions  = get_func_source_files(mode="none", pattern=PATTERN_LH_FUNCTIONS)
    pprint(lh_functions)


def test_print_all_functions():
    all_functions = get_func_source_files(mode="none", pattern=PATTERN_ALL_FUNCTIONS)
    pprint(all_functions)


if __name__ == "__main__":
    test_regex_patterns()