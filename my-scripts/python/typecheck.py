from os.path import join
import re
import subprocess as subp

### FUNCTIONS FOR TYPE CHECKING ###
def add_ignore(dep, src_file):
    print(f"\tAdding {dep} to ignored functions")
    ignore_stmt = "\n{-@ ignore " + dep + "@-}"
    with open(join(path, src_file), 'a') as f:
        f.write(ignore_stmt)

def remove_ignores():
    cmd = r"sed -i '' '/{-@ ignore.*@-}/d' " + f"{path}/*.hs"
    subp.run(cmd, shell=True, check=True)

def run_typecheck(error_str="LIQUID: UNSAFE"):
    # alternative: out = subp.check_output(cmd, shell=True)
    out = subp.run(cmd, shell=True, check=True, capture_output=True, text=True)
    out = out.stdout + out.stderr
    if error_str in out:
        print('\tFailed to typecheck')
    else:
        all_checks = re.findall(r"\([0-9]+", out)
        num_checks = sum(list(map(lambda s: int(s[1:]), all_checks)))
        print(f'\t{num_checks} total constraints checked')
    return out

# run all typechecks
def test(dependencies, src_files):
    run_typecheck()
    for i, dep in enumerate(dependencies):
        print(f'Test {i+1}')
        if dep: add_ignore(dep, src_files[dep])
        out = run_typecheck()
        with open(f'../outputs/test{i+1:02d}.txt', 'w') as f:
            f.write(out)
    remove_ignores()
### END FUNCTIONS FOR TYPE CHECKING ###

if __name__ == "__main__":
    path = '../../Data' # '../../Data/ByteString'
    cmd = f"stack exec ghc -- -fplugin=LiquidHaskell {path}/*.hs; rm {path}/*.hi; rm {path}/*.o;"