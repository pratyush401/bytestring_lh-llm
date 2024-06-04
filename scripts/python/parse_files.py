import os 
from os.path import join
import json
import csv
import re
import ast
import pprint as pprint
from collections import defaultdict

### CONSTANTS ###
ROOTDIR = "Data"
PATTERN_LH_FUNCTIONS  = r"(?:{-@ )([^\s]+?)(?:\n?\s*::)"
PATTERN_ALL_FUNCTIONS = r"(?:\n\s*)([^\s-]+)(?:\n?\s*::)"
PATTERN_FUNC_CONTENTS = r"(?:\n?\s*::[^\n]*\n)(.*?)(?:\n\n|$)"

### HELPER FUNCTIONS FOR FILES ###
def read_csv_to_dict(fname):
    with open(fname, "r") as f:
        reader = csv.reader(f)
        data_dict = { row[0]: ast.literal_eval(row[1]) for row in reader }
    return data_dict

def read_list(fname):
    with open(fname, "r") as f:
        return f.read().splitlines()

def read_json(fname):
    with open(fname, "r", encoding="utf-8") as f:
        return json.loads(f.read())

# given a root directory, recursively iterate through files
# and subdirectories, adding their full paths to a list
def walk_directory(rootdir="Data"):
    file_list = []
    for path, _, files in os.walk(rootdir):
        for file in files:
            if file[-3:] == ".hs":
                target_file = join(path, file)
                file_list.append(target_file)
    return file_list

# given a path to a file, return the list of file paths stored in the file
def get_file_list(fpath="scripts/inputs/file_order_with_lh.txt"):
    return read_list(fpath)
### END HELPER FUNCTIONS ###


def get_func_source_files(
        mode="write", 
        fname="scripts/outputs/source_files.csv", 
        pattern=PATTERN_ALL_FUNCTIONS,  
        key="function", 
        file_list=None
    ):

    if mode == "read":
        return read_csv_to_dict(fname)

    source_files = defaultdict(list)
    if file_list is None:
        file_list = get_file_list()

    for target_file in file_list:
        fcontents = open(target_file, "r").read()
        all_functions = re.findall(pattern, fcontents)
        
        if key == "file":
            source_files[target_file] = all_functions
        else: # use function name as dict key
            [source_files[func].append(target_file) for func in all_functions]

    if mode == "write":
        with open(fname, "w") as f:
            w = csv.writer(f)
            w.writerows(source_files.items())
    return source_files


# requires either file_contents or file_name to be passed in
def get_function_contents(fn_name, file_contents="", file_name=None):
    if file_name is not None:
        file_contents = open(file_name).read()
    pattern = f"\n({fn_name}){PATTERN_FUNC_CONTENTS}"
    match = re.findall(pattern, file_contents, re.DOTALL)
    return match


# reads in json file of dependencies and constructs a list in reverse order for type checking
def get_dependencies_from_file(fname="scripts/inputs/dependency.json", reverse=True):
    dependencies = read_json(fname)
    dependencies = list(dependencies.keys())
    if reverse:
        dependencies = list(reversed(dependencies))
    dependencies = list(map(lambda s: s.strip(), dependencies)) # remove trailing space
    return dependencies


def construct_dependencies(file_funcs, file_order, lh_functions, output_dir='scripts/outputs/'):
    dependencies = defaultdict(list)
    refinement_types = dict()

    # iterate once to construct list of functions and 
    # whether they have a refinement type
    for file in file_order:
        for func in file_funcs[file]:
            dependencies[f"{file}--{func}"] = []

            # record whether function has refinement type
            has_refinement_type = func in lh_functions
            refinement_types[f"{file}--{func}"] = has_refinement_type

    # iterate second time to check for each function, 
    # which function names (including itself?) appear in its body
    for file in file_order:
        # get contents of current file in file order
        fcontents = open(file, "r").read() 

        # get list of functions in current file
        for func in file_funcs[file]:
            # get current function body's if it can be found
            func_contents = get_function_contents(func, fcontents)
            if not func_contents: 
                print(f"Empty function contents for {func} in {file}")
                continue
            elif len(func_contents) > 1:
                print(f"{len(func_contents)} matches found for {func} in {file}")
            func_contents = func_contents[0][1] # take the first match/definition 

            # check if previous functions are called in current function's body 
            dependencies[f"{file}--{func}"] = []
            for dep in dependencies: # iterate through all functions previously been checked
                dep_file, dep_func = dep.split("--")
                
                # TODO recursive calls vs function name appearing in comment/definition
                if func == dep_func: continue

                # skip for now TODO figure out what to do with these later
                if (dep_func == '(>$<)' 
                    or dep_func == '(>*<)' 
                    or dep_func == '(!?)'
                    or dep_func == 'go'
                    or dep_func == "go'"
                ):
                    continue

                # search function name (not surrounded by any word characters or ticks)
                # to avoid cases like foldl being counted as a dependency with foldl'
                match = re.search(f"[^\w]{dep_func}[^\w']", func_contents)
                if match:
                    dependencies[f"{file}--{func}"].append([dep_file, dep_func])
 
    print("Total number of functions:", len(dependencies))
    with open(f"{output_dir}bytestring_dependencies.json", "w") as json_file:
        json.dump(dependencies, json_file, indent=4)

    with open(f"{output_dir}bytestring_refinement_types.json", "w") as json_file:
        json.dump(refinement_types, json_file, indent=4)


# run this script from bytestring_lh-llm dir
# > python3 scripts/python/parse_files.py
if __name__ == "__main__":
    lh_functions  = get_func_source_files(mode="none", pattern=PATTERN_LH_FUNCTIONS)
    all_functions = get_func_source_files(mode="write", fname="scripts/outputs/source_files.csv", pattern=PATTERN_ALL_FUNCTIONS, key="function")
    
    file_funcs = get_func_source_files(mode="write", fname="scripts/outputs/Data_directory.csv", key="file", file_list=walk_directory())
    file_order = read_list("scripts/inputs/file_order_with_lh.txt")

    construct_dependencies(file_funcs, file_order, lh_functions)
