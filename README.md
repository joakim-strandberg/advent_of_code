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
by the GNAT compiler:
```
pragma Restrictions (No_Dependence => Ada.Unchecked_Deallocation);
```
##### Prevention of memory leaks in Ada83
Here is an example of an application with memory leak in Ada83
or MIL-STD-1815 from 1980:
```
procedure Demo_Of_Memory_Leak is
   type Integer_Access is access Integer;
   I : Integer_Access := new Integer'(5);
begin
   null;
end Demo_Of_Memory_Leak;
```
The application allocates an integer in the standard storage pool which is
usually the heap. At application exit the memory is never deallocated.
Note that the access-to-object type named Integer_Access only exists locally
inside the procedure. In Ada83 it is possible to associate the
access-to-object type with a pool of memory with fixed size that will be
automatically deallocated at the point when the access-to-object type
ceases to exist. To specify that a memory pool with a size of 16 bytes are
to be associated with the Integer_Access type the syntax is
"for Integer_Access'Storage_Size use 16;". The memory safe application is:
```
procedure Demo_Safe_Application is
   type Integer_Access is access Integer;
   for Integer_Access'Storage_Size use 16;
   I : Integer_Access := new Integer'(5);
begin
   null;
end Demo_Safe_Application;
```
To make sure an Ada83 application is memory safe, one way to verify it is
to check that no memory is ever deallocated using Unchecked_Deallocation
and making sure all defined access-to-object types are used together
with the 'Storage_Size attribute. Either one can do this manually or develop
an application that analyzes the Ada source code and verifies it.

##### Prevention of memory leaks in Ada95
In Ada95 the 'Storage_Size attribute was deprecated in favor of using
Storage pools introduced in the Ada95 standard and is the method used
in the source code of this repository. Instead of verifying each
access-to-object type definition is associated with the 'Storage_Size
attribute, it needs to be verified that each access-to-object type definition
is instead associated with the 'Storage_Pool attribute. The author Joakim
Strandberg has manually verified each access-to-object definition is
associated with the 'Storage_Pool attribute. In addition, the valgrind
application has been utilized on Linux for this purpose as well.

#### How risk of aliasing has been minimized or prevented
The SPARK tools can detect any aliasing issue in SPARK code.
Without such static code analysis support, there is a risk in Ada code that
aliasing issues may occur. One research done in the 90s suggested that
the reason Ada applications are more robust compared to applications
written in other programming languages is that the Ada language is better
at preventing aliasing issues. However, the risk still exists.

The risk is minimized first by unit tests. If aliasing issues has generated
unintended machine code there is a chance this is discovered by the unit
tests. Secondly, AdaControl can detect aliasing issues but not all.
The rules for aliasing detection by AdaControl is:
```
check parameter_aliasing (with_in certain);
search parameter_aliasing (Possible);
```
