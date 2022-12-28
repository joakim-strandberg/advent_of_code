# Advent of Code
This repository contains solution to challenges presented at
[Advent of Code](https://adventofcode.com/).
The puzzles presented at Advent of Code are designed to be solvable
within 10 seconds on 10 years old hardware.

All the source code in this repository can be compiled by any Ada compiler
that implements the Ada95 standard from 1995.
The code has been successfully compiled using GNAT Community Edition 2021
(Windows 10 and Ubuntu 18.04), Janus/Ada 3.2.1 from June 2019 (Windows 10),
GNAT 3.14p from October 2004 (Windows 10) and GNAT 3.14b on FreeDOS.
In addition, all the code in this repository can be compiled by the ObjectAda
7.0 compiler from 1996 on Windows 95 but generates a faulty
executable (the cause is a bug related to Storage pool usage in the Ada source
code).

## Building the source code
Building the source code results in an executable which is a
command line interface application.

The executable's name is **aoc** on Mac OS X and
Linux but will be **aoc.exe** on Windows.
The executable will be generated in the /bin directory.
Execute the **aoc** or **aoc.exe** application without
arguments to read about instructions on how to use it.

### The Janus/Ada compiler on Windows 10
Open a command terminal in Windows. Make sure the "obj_janus" directory exists:
```
mkdir obj_janus
```
Execute:
```
cmain.bat
```
This will produce a file called bmain.bat. Execute:
```
bmain.bat
```
The executable will be generated in the /bin directory.
## The GNAT compiler on Windows, MAC OS X or Linux
Execute:
```
gprbuild -p -P main.gpr
```
The executable will be generated in the /bin directory.
### The GNAT 3.14p compiler (2004) on Windows or Linux
At the time when this compiler was released, the gprbuild application
and cross-language build system didn't exist yet and the way to build
Ada source code with the GNAT compiler was gnatmake. Execute:
```
gnatmake -aI private/ main.adb
```
### The GNAT 3.14b (2004) compiler on FreeDOS
Execute:
```
gnatmake -aI private/ main.adb
```
## Why Ada95?
If one writes Ada code today in 2022, it would be natural to restrict oneself
to the subset of Ada called SPARK and which enables verification by a
toolset from AdaCore using formal methods. The result is ultra-low defect
software. It is for those who like to use a programming language that
maximises safety and security guarantees, for example:
 - Memory safety. All allocated memory will be deallocated.
   No memory leaks possible. Any aliasing issues are detected.
 - No infinite loops. All loops are mathematically proven to finish iteration
   within a finite number of steps.
 - The Ada/SPARK code is exception free. It will be safe to suppress
   run-time error checking the Ada compiler itself is unable to optimize away.
   Maximum safety enables maximum performance.
It has been possible to use the SPARK tools since 2014. However, the first
version of Ada came out in 1980 and was described in the MIL-STD-1815
standard. The first Ada ISO standard is called Ada83 and was released in 1983.
It is not possible to take SPARK code written today and compile the code
using a compiler prior to 2014 because not only does SPARK build on the
Ada2012 standard but also adds constructs outside the Ada standard, and
which are necessary to enable proof of algorithm properties.

The advent of code challenges are designed to be solved by 10 year old
hardware and it is therefore natural to consider sticking to an older version
of the Ada standard. The Ada95 standard is the World's first object oriented
programming language and already a very expressive language.
The Ada95 standard also offers maximum portability.
The source code in this repository demonstrates and gives insight into
how the life of an Ada developer was in the end of the 90's.
It is also illuminating to compare how the Ada language has evolved since
then.

## Ada programming language restrictions
It is here described the subset of the Ada95 standard which has been used.

### Filenaming convention
The filenames follow the 8.3 filenaming convention used in MS-DOS.
It means each file filename is maximum 8 ASCII characters followed by
optionally a filename extension consisting of a period . and at most three
further characters. No need to change any filenames to transfer the files
to an MS-DOS computer.

### Maximum line length and Ada source code file size
The maximum line length is 80 characters per line of code. This is verified
by AdaControl.

Each Ada source code file is less than 65kB. This means that it is possible
to edit each Ada source code file with the edit.exe application,
first release was in 1991, on MS-DOS or FreeDOS.
The edit application stores the whole text file in a byte array
where the byte array index is a 16-bit unsigned integer. It means all the
source code in this repository could have been implemented on MS-DOS
together with an Ada95 compiler.

The Ada code in this repository demonstrates how it is possible to
use the **private package** feature introduced in the Ada95 standard to build
large code bases by splitting up source code in files with a maximum file size
(see for example the implementation of UTF8 in the Std.UTF8 package).

### Memory safety
The SPARK toolset find memory leaks and aliasing issues but requires
SPARK source code. The source code in this repository is Ada95 which means
the SPARK toolset cannot be used to verify there are no memory leaks
or aliasing issues.
#### How memory leaks has been prevented
All memory is statically allocated at application startup and there are no
deallocations of memory during run-time. No deallocations are verified
by the GNAT compiler by the restriction:
```
pragma Restrictions (No_Dependence => Ada.Unchecked_Deallocation);
```

#### How risk of aliasing has been prevented
