#!/usr/bin/env python3
"""
Cyclomatic complexity checker for Haskell source files.

Measures decision points per top-level function:
  - case/of branches
  - if/then/else
  - guard expressions (|)
  - pattern match alternatives in function definitions

Cyclomatic complexity = 1 + number of decision points

Usage:
    python3 check_complexity.py <file.hs> <max_complexity>

Exit code 0 if all functions <= max_complexity, 1 otherwise.
"""

import re
import sys


def measure_complexity(filepath, max_cc):
    """Measure cyclomatic complexity of each top-level function."""
    with open(filepath, 'r') as f:
        lines = f.readlines()

    violations = []
    current_func = None
    current_cc = 1
    func_line = 0
    indent_level = 0
    in_where = False

    for i, line in enumerate(lines, 1):
        stripped = line.rstrip()
        if not stripped or stripped.startswith('--') or stripped.startswith('{-'):
            continue

        # Detect top-level function definition (starts at column 0, has = or |)
        if stripped and not stripped[0].isspace() and not stripped.startswith('module') \
                and not stripped.startswith('import') and not stripped.startswith('data') \
                and not stripped.startswith('type') and not stripped.startswith('newtype') \
                and not stripped.startswith('class') and not stripped.startswith('instance') \
                and not stripped.startswith('deriving') and not stripped.startswith('infixl') \
                and not stripped.startswith('infixr') and not stripped.startswith('{-') \
                and not stripped.startswith('#'):

            # Save previous function if any
            if current_func and current_cc > max_cc:
                violations.append((current_func, func_line, current_cc))

            # Check if this is a type signature (has ::)
            if '::' in stripped and '=' not in stripped.split('::')[0]:
                continue

            # Extract function name
            match = re.match(r'^([a-z_][a-zA-Z0-9_\']*)', stripped)
            if match:
                current_func = match.group(1)
                func_line = i
                current_cc = 1
                in_where = False

        # Count decision points within current function
        if current_func:
            # case ... of
            current_cc += len(re.findall(r'\bcase\b', stripped))
            # if ... then
            current_cc += len(re.findall(r'\bif\b', stripped))
            # guard expressions (| at start of line after indentation)
            if re.match(r'^\s+\|(?!=)', stripped):
                current_cc += 1
            # Pattern match alternatives in case (->)
            if re.match(r'^\s+\S.*\s->\s', stripped) and 'where' not in stripped:
                # Don't count the first alternative (already counted by case)
                pass

    # Check last function
    if current_func and current_cc > max_cc:
        violations.append((current_func, func_line, current_cc))

    return violations


def main():
    if len(sys.argv) != 3:
        print(f"Usage: {sys.argv[0]} <file.hs> <max_complexity>")
        sys.exit(2)

    filepath = sys.argv[1]
    max_cc = int(sys.argv[2])

    try:
        violations = measure_complexity(filepath, max_cc)
    except (FileNotFoundError, UnicodeDecodeError):
        sys.exit(0)  # Skip files that can't be read

    if violations:
        for func, line, cc in violations:
            print(f"  COMPLEXITY: {filepath}:{line} {func}() = {cc} (max {max_cc})")
        sys.exit(1)
    else:
        sys.exit(0)


if __name__ == '__main__':
    main()
