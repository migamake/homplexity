1. Function length (recommended 4-40 lines of code for C, except for multiway branches.)
2. Module length (recommended 4-400 lines for C, 10-40 functions per file)
3. Ratio of comments (30-75% of code length, excluding blank lines. Exception is C header file.)
2. Number of arguments
3. Number of local definitions
4. Complexity measures:
  * McCabe "Cyclomatic number" - number of linearly independent paths through code, tends to be PL independent
    # ~number of conditional branches (ifs, cases) that may be taken,
    # also iteration constructs (like foldr, not map!)
    # also exception catches
    # also || and && shortcut evaluations?
    -> should be <15 for a function
    -> should be <100 for a file
  * Halstead Metrics
    # volume: number of operators+operands * log2 of number of unique operators+operands?
  * Maintainability Index
5. Number of links between functions and modules (n-planarity of the graph)
6. Editing distance between different exposed names
7. Warn about tuples with more than 5 arguments?
8. Warn about constructors with more than 4 arguments that are not records.
9. Facilities:
  * Iterable instances for haskell-src-exts (so that there is convenient interface to quickly
check all source instances within given structure, for example).
  * Lens instances.
