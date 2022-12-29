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
### The GNAT compiler on Windows, MAC OS X or Linux
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
### Sanity check on the generated binary
The source code contains unit tests.
If the generated machine code is correct all tests should pass.

On Mac OS X or Linux:
```
cd bin
./aoc tests
```
On Windows:
```
cd bin
aoc.exe tests
```
Example of output on Linux:
```
...$ ./aoc tests
Unit tests of the Stda and Stdb packages
--------------------------------------------------------------------------------
Split string test 'A B', separator ' ':                                      OK
Split string test ' A B', separator ' ':                                     OK
Split string test 'A B ', separator ' ':                                     OK
Split string test 'A  B', separator ' ':                                     OK
Split string test 'AA B', separator ' ':                                     OK
Split string test ' AA B', separator ' ':                                    OK
Split string test 'AA B ', separator ' ':                                    OK
Split string test 'AA  B', separator ' ':                                    OK
Split string test 'A BB', separator ' ':                                     OK
Split string test ' A BB', separator ' ':                                    OK
Split string test 'A BB ', separator ' ':                                    OK
Split string test 'A  BB', separator ' ':                                    OK
Ada code location test (1625143615, 0529906411)):                            OK
Ada code location test (0000000001, -0000000005)):                           OK
Ada code location test (-0000000013, 0000000487)):                           OK
Ada code location test (-2022574034, -0087287826)):                          OK
Total number of tests:16
Tests failed:0
Tests passed:16

Unit tests of the puzzles for Advent of Code 2021
--------------------------------------------------------------------------------
Day 01, part 1. Result should be 7:                                          OK
Day 01, part 2. Result should be 5:                                          OK
Day 02, part 1. Result should be 150:                                        OK
Day 02, part 2. Result should be 900:                                        OK
Day 02, part 1. Result should be 198:                                        OK
Day 04, part 1. Result should be 4512:                                       OK
Day 05, part 1. Result should be 5:                                          OK
Total number of tests:7
Tests failed:0
Tests passed:7

Unit tests of the puzzles for Advent of Code 2022
--------------------------------------------------------------------------------
Day 01, part 1. Result should be 24000:                                      OK
Day 01, part 1. Result should be 71924:                                      OK
Day 01, part 1. Result should be 45000:                                      OK
Day 01, part 1. Result should be 210406:                                     OK
Day 02, part 1. Result should be 15:                                         OK
Day 02, part 1. Result should be 12:                                         OK
Day 03, part 1. Result should be 157:                                        OK
Day 03, part 2. Result should be 70:                                         OK
Total number of tests:8
Tests failed:0
Tests passed:8
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

### Source code is expressed in ASCII
Modern software today is often written in UTF8. However, in UTF8 there
may be several different "characters" that look the same but are different
(different code points). To avoid any confusion and maximize portability,
the ASCII character set is used to express the Ada source code.
This does not preclude possibility to develop applications that support UTF8.
See for example the Std.UTF8 package. The idea is to keep things simple.

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

### No race-conditions
The resulting binary from compilation from Ada source code has no possibility
of any race-conditions. This is ensured by that the application is single
task and is verified at compile-time by the GNAT application by:
```
pragma Restrictions (Max_Tasks => 0);
```
What this says is that there are no extra tasks in addition to the
environment task which is the application which calls application entry.

### Memory safety
The SPARK toolset find memory leaks and aliasing issues but requires
SPARK source code. The source code in this repository is Ada95 which means
the SPARK toolset cannot be used to verify there are no memory leaks
or aliasing issues.

#### How memory leaks have been prevented
All memory is statically allocated at application startup and there are no
deallocations of memory during run-time. That there are no deallocations
are verified by the GNAT compiler:
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

If other features of Ada is used for example to make heap allocations
during run-time other methods must be used to verify there are no
memory leaks. What is described above is very close to how it is verified
in the Ada source code of this repository there are no memory leaks.

##### Prevention of memory leaks in Ada95
In Ada95 the 'Storage_Size attribute was deprecated in favor of using
Storage pools introduced in the Ada95 standard and is the method used
in the source code of this repository. Instead of verifying each
access-to-object type definition is associated with the 'Storage_Size
attribute, it needs to be verified that each access-to-object type definition
is instead associated with the 'Storage_Pool attribute. The author of this
text has manually verified each access-to-object definition is
associated with the 'Storage_Pool attribute. In addition, the valgrind
application has been utilized on Linux for this purpose as well.
No memory leak is expected and no memory leak has been detected.

##### The necessity of using Storage pools
There is a very strong tradition within the Ada community to work on the stack
only and never make any heap allocations. The way it is usually done
is to create a task and specify the size of the stack. Does the application
need 4GB of memory to work properly then 4GB will be the stack size
of the task doing the work. Unfortunately, the environment task which is
the task that calls the Main procedure or application entry procedure
is a task with a stack size set by the Operating System and cannot be
specified from within the Ada language. Secondly, not all Ada compilers
support specifying the stack size at task creation. To make the Ada code
in this repository cross-compiler Storage pools are used instead to
overcome the potential stack size limitation.

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
AdaControl does not detect any aliasing issues.
However, it does not detect all possible causes like for example in
the following article by Florian Weimer.
##### A Hole in Ada Type Safety
Here Florian Weimer's article is published in its entirety from:
```
https://www.enyo.de/fw/notes/ada-type-safety.html.
```
**Ada aims to restrict unsafe language features which break type safety
to a set of constructs which can be easily spotted in source case.
Examples include unchecked deallocation, unchecked conversion and explicit
address handling. This note shows that a combination of safe-looking
language features can be used to undermine type safety, too.**

The standard way to show that type safety has been broken in a language
is to implement a cast function Conversion from any type
Source to any type Target:
```
generic
   type Source is private;
   type Target is private;
function Conversion (S : Source) return Target;
```
This generic function can be used like Ada.Unchecked_Conversion:
```
with Ada.Text_IO;
with Ada.Unchecked_Conversion;

with Conversion;

procedure Convert_Test is
   type Integer_Access is access all Integer;

   J : aliased Integer;
   J_Access : Integer_Access := J'Access;

   function Convert is new Conversion (Integer_Access, Integer);
   function Unchecked_Convert is new Ada.Unchecked_Conversion
     (Integer_Access, Integer);

begin
   Ada.Text_IO.Put_Line (Integer'Image (Convert (J_Access)));
   Ada.Text_IO.Put_Line (Integer'Image (Unchecked_Convert (J_Access)));
end Convert_Test;
```
How can we implement Conversion? It turns out that discriminant records
with defaults combined with aliasing do the trick:
```
function Conversion (S : Source) return Target is
   type Source_Wrapper is tagged record
      S : Source;
   end record;
   type Target_Wrapper is tagged record
      T : Target;
   end record;

   type Selector is (Source_Field, Target_Field);
   type Magic (Sel : Selector := Target_Field) is record
      case Sel is
	 when Source_Field =>
	    S : Source_Wrapper;
	 when Target_Field =>
	    T : Target_Wrapper;
      end case;
   end record;

   M : Magic;

   function Convert (T : Target_Wrapper) return Target is
   begin
      M := (Sel => Source_Field, S => (S => S));
      return T.T;
   end Convert;

begin
   return Convert (M.T);
end Conversion;
```
When the inner function Convert is called, the discriminant Sel of M has
the value Target_Field, thus the component M.T can be dereferenced.
The assignment statement in Convert changes the discriminant and the value.
But the source value S is still reachable as an object of type Target
because the parameter T aliases the component M.T, so the return statement
executes without raising an exception.

The two tagged records are used to force that the inner Convert function
receives its parameter by reference. Without them, an implementation would
be free to pass the discriminant record Magic by copy,
and aliasing would not occur.

Our implementation lacks the full power of Ada.Unchecked_Conversion because
it does not support limited or unconstrained types. However, it is sufficient
to break type safety.

Addendum: Robert A Duff noted that this was already known in
the Ada 83 timeframe. According to paragraph 3.7.2(4)
```
http://www.adaic.org/resources/add_content/standards/95aarm/AARM_HTML/AA-3-7-2.html#I1973
```
in the standard (Ada 95 Annotated Reference Manual with
Technical Corrigendum 1, MITRE Corporation, 2000, retrieved 2011-04-30),
the call to Convert is erroneous on its own, and not the return statement
which uses the stale reference.

Addendum (2015): A Rust variant (for the unsafe language extension)
of this approach has been published as A Type Safety Hole in Unsafe Rust).

##### How to prevent: A Hole in Ada Type Safety
What Florian Weimer's article shows is how safe looking features of Ada can
be combined to break the type safety of the Ada language. Is there anything
that can be done to avoid the hole in type safety?
One feature that is key to enable the described type safety hole is
the existence of discriminants with default values. They are therefore
banished from the subset of Ada used in this repository and this is verified
using AdaControl:
```
--  No usage of discriminants with default values.
check declarations (defaulted_discriminant);
```
##### The traditional way to go outside Ada's Type Safety
A language that greatly inspired the design of the Ada language is Pascal.
One of the great criticisms of that language is that there was no way
to circumvent the type system. In Pascal there is no escape.
When Ada was designed it was therefore important to be able to circumvent
the type system and that's why Unchecked_Conversion was included in Ada83.
Using Unchecked_Conversion it is possible to reinterpret the representation
of an object in memory as an object of another type. The function name
is prefixed with the word "Unchecked" to indicate it is potentially unsafe
to use. If Ada was designed today maybe the word "Unsafe" would have been
used instead.

Another way that circumvents Ada's strict type system is the possibility
to reinterpret a memory address as an access-to-object type by using
the package System.Address_To_Access_Conversions and was introduced in Ada95.

It is verified by the GNAT compiler these packages are not used anywhere:
```
pragma Restrictions (No_Dependence => Ada.Unchecked_Conversion);
pragma Restrictions (No_Dependence => System.Address_To_Access_Conversions);
```
#### Other restrictions
There is no usage of 'Access or 'Unchecked_Access nor the 'Address attribute.
This is checked by AdaControl:
```
check entities (all 'Address);
check entities (all 'Access);
check entities (all 'Unchecked_Access);
```
# Biggest design mistakes in the Ada language?
Two candidates are discriminants with default values and the other is
the support for streaming.
## Discriminants with default values
Consider the declaration of the Variable_String type:
```
subtype String_Index is Positive range 1 .. 9;
type Variable_String (Last : String_Index) is record
      Text : String (1 .. Last);
   end record;
```
When declaring an instance of the Variable_String type the discriminat Last
must be given a value:
```
   S : Variable_String (5);
```
After the declaration of S it is not possible to change the value of the
discriminant Last. Note also that Last always must be given a value.
If a value has not been specified by mistake the compiler will generate
an error message:
```
   T : Variable_String;  --  Generates compiler error
```
If instead Last is given a default value, it is suddenly OK to declare
instances without always specifying a value for the discriminant.
```
subtype String_Index is Positive range 1 .. 9;
type Variable_String2 (Last : String_Index := 9) is record
      Text : String (1 .. Last);
   end record;

   R : Variable_String2;  -- Is OK, does not generate compiler error
```
Not only is it OK to declare instances of the Variable_String2 type but
it is also possible to change the value of Last after declaration:
```
   R := (Last => 3, Text => (1 => 'a', 2 => 'b', 3 => 'c'));
```
It means that adding the default value ":= 9" to the discriminant
completely changes the semantics of the record declaration.
This seems to contradict the principle of least surprise.
In addition, the existence of default discriminants introduces complexity
because there are special cases. For example, when tagged record types were
introduced in Ada95 they were not allowed to have discriminants with
default values. In Ada2012 this was relaxed to allow discriminants with
default values on limited tagged record types. To keep things simple,
usage of discriminants with default values are avoided in the source code
of this repository.
