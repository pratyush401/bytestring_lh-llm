import os # for directory walk
from os.path import join
import json
import csv
import re
from collections import defaultdict
import ast
import pprint as pprint

### HELPER FUNCTIONS FOR READING FILES ###
def read_csv_to_dict(fname):
    with open(fname, 'r') as f:
        reader = csv.reader(f)
        data_dict = { row[0]: ast.literal_eval(row[1]) for row in reader }
    return data_dict

def read_list(fname):
    with open(fname, 'r') as f:
        return f.read().splitlines()

def read_json(fname):
    with open(fname, "r", encoding="utf-8") as f:
        return json.loads(f.read())
### END HELPER FUNCTIONS ###

# pattern, indices, and key are only used in write mode
def get_func_source_files(mode='write', 
                          fname='my-scripts/inputs/source_files.csv', 
                          pattern=r"(?:{-@\s+)(\S+)(?:\s+::)",  
                          key='file'
                        ):
    if mode == 'read':
        return read_csv_to_dict(fname)

    source_files = defaultdict(list)
    for path, dirs, files in os.walk(rootdir):
        for file in files:
            if file[-3:] == ".hs":
                target_file = join(path, file)
    for target_file in read_list('my-scripts/inputs/file_order_with_lh.txt'):
                with open(target_file, "r") as f:
                    fcontents = f.read()
                    all_functions = re.findall(pattern, fcontents)
                    
                    if key == "file":
                        source_files[target_file] = all_functions
                    else:
                        # use function name as key
                        for func in all_functions:
                            source_files[func].append(target_file)
    if mode == 'write':
        out_fname = fname.replace('inputs', 'outputs')
        with open(out_fname, 'w') as f:
            w = csv.writer(f)
            w.writerows(source_files.items())
    return source_files

# requires either file_contents or file_name to be passed in
def get_function_contents(fn_name, file_contents='', file_name=None):
    if file_name is not None:
        file_contents = open(file_name).read()
    pattern = f"\n({fn_name})(?:\n?\s*::[^\n]*\n)(.*?)(?:\n\n|$)"
    match = re.findall(pattern, file_contents, re.DOTALL)
    return match

# reads in json file of dependencies and constructs a list in reverse order for type checking
def get_dependencies_from_file(fname='../inputs/final_dependency.json', reverse=True):
    dependencies = read_json(fname)
    dependencies = list(dependencies.keys())
    if reverse:
        dependencies = list(reversed(dependencies))
    dependencies = list(map(lambda s: s.strip(), dependencies)) # remove trailing space
    return dependencies

def construct_dependencies():
    file_funcs = get_func_source_files(mode='read', fname='my-scripts/inputs/Data_directory.csv')
    file_order = read_list('my-scripts/inputs/file_order_with_lh.txt')
    
    dependencies = defaultdict(list)
    for file in file_order:
        fcontents = open(file).read() # get contents of current file in file order

        for func in file_funcs[file]: # get list of functions in current file
            func_contents = get_function_contents(func, fcontents)
            if not func_contents: 
                print(f"Empty function contents for {func} in {file}")
                continue
            elif len(func_contents) > 1:
                print(f"{len(func_contents)} matches found for {func} in {file}")
        
            func_contents = func_contents[0][1]
            dependencies[f"{file}--{func}"] = []
            for dep in dependencies: # iterate through all functions that have previously been checked
                dep_file, dep_func = dep.split("--")
                if dep_func != func and dep_func in func_contents:
                    has_refinement_type = "{-@ " + func in lh_functions
                    dependencies[f"{file}--{func}"].append([dep_func, dep_file, has_refinement_type])

    print('total number of functions:', len(dependencies))
    with open('my-scripts/outputs/dependencies.json', 'w') as json_file:
        json.dump(dependencies, json_file, indent=2)

if __name__ == "__main__":
    # run this script from bytestring_lh-llm dir
    # > python3 my-scripts/python/parse_files.py
    p = pprint.PrettyPrinter()

    rootdir = 'Data'
    pattern_lh_functions = r"({-@ [^\s]+?)(?:\n?\s*::)"
    pattern_functions = r"(?:\n)([^\s-]+)(?:\n?\s*::)"

    lh_functions = get_func_source_files(mode='none', pattern=pattern_lh_functions, key='function')
    all_functions = get_func_source_files(mode='write', pattern=pattern_functions, key='function', fname='my-scripts/outputs/source_files.csv')

def compare_regex_results():
    pattern0 = r"(?:\n)(.+)(?:\n?\s*::)"
    pattern1 = r"(?:\n)(.+?)(?:\s*)(?:\n?\s*::)"
    pattern2 = r"(?:\n)([^\s]+\s?[^\s]+)(?:\n?\s*::)"
    pattern3 = r"({-@ [^\s]+?)(?:\n?\s*::)"
    pattern4 = r"(?:(?:\n)|(?:{-@ ))([^\s-]+)(?:\n?\s*::)"
    pattern5 = r"(?:\n)([^\s-]+)(?:\n?\s*::)"

    result1 = get_func_source_files(mode='write', pattern=pattern0, key='function', fname='my-scripts/outputs/result1.csv')
    result2 = get_func_source_files(mode='write', pattern=pattern5, key='function', fname='my-scripts/outputs/result2.csv')
    set1 = set(map(lambda s: s.strip().replace("{-@ ", ""), result1))
    set2 = set(map(lambda s: s.strip(), result2))

    print()
    print('items in set1', len(set1))
    print('items in set2', len(set2))

    print()
    diff = set1.difference(set2)
    p.pprint(diff)
    print('number of functions excluded', len(diff))

    print()
    diff = set2.difference(set1)
    p.pprint(diff)
    print('number of functions excluded', len(diff))

    print()
    intersect = set1.intersection(set2)
    p.pprint(intersect)
    print('number of functions shared', len(intersect))

    
def test_get_function_contents():
    function_source_files = read_csv_to_dict('my-scripts/inputs/function_source_files.csv')
    p.pprint(get_function_contents('word64BE', file_name=function_source_files['word64BE'][1])[0][1])