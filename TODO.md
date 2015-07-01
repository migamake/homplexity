- [x] Function length (recommended 4-40 lines of code for C, except for multiway branches.)
- [x] Module length (recommended 4-400 lines for C, 10-40 functions per file)
- [ ] Ratio of comments (30-75% of code length, excluding blank lines. Exception is C header file.)
- [ ] https://en.wikipedia.org/wiki/Flesch–Kincaid\_readability\_tests
- [x] Number of arguments
- [ ] Number of local definitions
- [ ] Complexity measures:
  * [x] McCabe "Cyclomatic number" - number of linearly independent paths through code, tends to be PL independent
    # [x] ~number of conditional branches (ifs, cases) that may be taken,
    # [-] also iteration constructs (like foldr, not map!?)
    # [ ] also exception catches
    # [-] also || and && shortcut evaluations?
    -> should be <15 for a function
    -> should be <100 for a file
  * [ ] Halstead Metrics
    # volume: number of operators+operands * log2 of number of unique operators+operands?
  * [ ] Maintainability Index
5. [ ] Number of links between functions and modules (n-planarity of the graph)
6. [ ] Editing distance between different exposed names
7. [ ] Warn about tuples with more than 5 arguments?
8. [ ] Warn about constructors with more than 4 arguments that are not records.
9. Facilities:
  * [x] Iterable instances for haskell-src-exts (so that there is convenient interface to quickly
check all source instances within given structure, for example).
  * [ ] Lens instances.
10. [x] Count errors, and show how many files couldn't be parsed.
11. [ ] Treat classes similar to records - limit number of symbols.
12. [ ] Try to guess extra params, or read them from file:
  * [x] cpphs
    # cpphs options to be snatched from Cabal
  * [ ] LANGUAGE pragmas
13. [x] Catch "invalid byte sequence" errors, and report as incorrect files.
