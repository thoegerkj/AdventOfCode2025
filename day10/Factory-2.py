from z3 import *
import re

INPUT_FILE = 'input.txt'

def factory_problem():
    with open(INPUT_FILE, 'r') as f:
        lines = f.readlines()
        solutions = []
        for line in lines:
            #print("Processing line:", line.strip())
            solutions.append(solve(line))
        #print(solutions)
        print("Solution:", sum(solutions))



def solve(line):
    (_, btns, jolts) = readMachineSpec(line)
    s = Optimize()
    num_buttons = len(btns)
    num_jolts = len(jolts)
    # Create Z3 integer variables for button presses
    button_vars = [Int(f'btn_{i}') for i in range(num_buttons)]
    # Add constraints for button presses
    for i in range(num_buttons):
        s.add(button_vars[i] >= 0)
    # Add constraints for joltages
    for i in range(num_jolts):
        s.add(jolts[i] == Sum([button_vars[b] for b in range(num_buttons) if i in btns[b]]))
    s.minimize(Sum(button_vars))

    if s.check() == sat:
        model = s.model()
        button_presses = [model[button_vars[i]].as_long() for i in range(num_buttons)]
        print(button_presses)
        return sum(button_presses)

    else:
        return None



# Vibede nedenstående med basis i min haskell-løsning
def split_commas(s):
    return [x for x in s.split(",") if x.strip()]

def inside_braces(open_c, close_c, s):
    """
    Extract substring inside matching open_c ... close_c,
    starting from the first occurrence of open_c.
    Returns (extracted_string, rest_after_closing).
    """
    start = s.find(open_c)
    if start == -1:
        return "", s
    depth = 0
    for i in range(start, len(s)):
        if s[i] == open_c:
            if depth == 0:
                content_start = i + 1
            depth += 1
        elif s[i] == close_c:
            depth -= 1
            if depth == 0:
                return s[content_start:i], s[i+1:]
    return "", s  # no matching close brace found


def readMachineSpec(s):
    # ------------------------------------------------------------
    # 1. Extract lights: a bracketed list of '.' and '#'
    # ------------------------------------------------------------
    # Equivalent to Haskell `lights`
    m = re.match(r"\s*\[([.#]*)\]", s)
    if not m:
        raise ValueError("Invalid or missing lights section `[ ... ]`")

    lights_str = m.group(1)
    rest = s[m.end():]

    # ------------------------------------------------------------
    # 2. Extract button lists: a sequence of `(x,y,...)`
    #    stopping before the `{...}` joltage section
    # ------------------------------------------------------------
    # Equivalent to Haskell `buttons`
    buttons = []
    while True:
        rest = rest.lstrip()
        if rest.startswith("{"):
            break  # end of button list

        if rest.startswith("("):
            nums_str, rest = inside_braces("(", ")", rest)
            nums = [int(x) for x in split_commas(nums_str)]
            buttons.append(nums)
        else:
            # skip non-brace characters (e.g., spaces)
            rest = rest[1:]

    # ------------------------------------------------------------
    # 3. Extract joltage list from `{...}`
    # ------------------------------------------------------------
    # Equivalent to Haskell `joltages`
    jolts_str, rest2 = inside_braces("{", "}", rest)
    joltages = [int(x) for x in split_commas(jolts_str)]

    return (lights_str, buttons, joltages)


if __name__ == "__main__":
    factory_problem()
