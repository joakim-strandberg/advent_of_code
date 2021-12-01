# Advent of Code
This repository contains solution to challenges presented at
[Advent of Code](https://adventofcode.com/).
The puzzles presented at Advent of Code are made in order to be solved
within 10 seconds on hardware 10 years old.

All the source code in this repository can be compiled by any Ada compiler
that implements at least the Ada95 standard from 1995.
The code has been successfully compiled using GNAT Community Edition 2021,
GNAT 3.14p from October 2004 and Janus/Ada 3.2.1.

# Building the source code
Building the source code will produce the executable aoc on Mac OS X and
Linux but will be aoc.exe on Windows. Execute the aoc application without
arguments to read about instructions on how to use it.

## Building the source code using the GNAT compiler
Execute:
'''
gprbuild -p -P main.gpr
'''
## Building the source code using GNAT 3.14p (from 2004)
Execute:
'''
gnatmake main.adb
'''
## Building the source code using Janus/Ada
Execute:
'''
cmain.bat
'''
This will produce a file called bmain.bat. Execute:
'''
bmain.bat
'''
