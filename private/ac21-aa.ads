with Ada.Finalization;

with System.Storage_Elements; use System;
with System.Storage_Pools;

with Std;
with Ada.Text_IO;

with Ac21_Tst;

package Ac21.Aa is

   use Std.Types;

   package Test_Suite renames Ac21_Tst.Test_Suite;

   package Nat32_IO renames Ac21_Tst.Nat32_IO;

   procedure Put      (Number : Nat32);
   procedure Put_Line (Number : Nat32);

   ----------------------------------------------------------------------------
   --
   --     Deepend - Dynamic Storage Pools for Ada 95, Ada 2005 and Ada 2012
   --
   --            B A S I C   B O U N D E D   D Y N A M I C   P O O L S
   --
   --                                S p e c
   --
   --                  Copyright (C) 2011, Bradley J. Moore
   --
   --  Deepend is free software;  you can  redistribute it  and/or modify it
   --  under  terms of the  GNU General Public License  as  published  by the
   --  Free Software  Foundation;  either version 2,  or (at your option) any
   --  later  version.  Paraffin is  distributed in the hope that it  will be
   --  useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
   --  MERCHANTABILITY  or  FITNESS  FOR  A  PARTICULAR PURPOSE.  See the GNU
   --  General Public License for  more details.  You should have  received a
   --  copy of the GNU General Public License distributed with Deepend;  see
   --  file  COPYING.  If  not,  write  to  the  Free  Software  Foundation,
   --  51 Franklin  Street,  Fifth  Floor, Boston, MA 02110-1301, USA.
   --
   --  As a  special exception, if other files  instantiate generics from
   --  this unit,  or you link this  unit with other files  to produce an
   --  executable,  this unit  does  not by  itself  cause the  resulting
   --  executable to be covered by  the GNU General Public License.  This
   --  exception does  not however invalidate  any other reasons  why the
   --  executable file might be covered by the GNU Public License.
   ----------------------------------------------------------------------------

   --  Deepend is a suite of dynamic storage pools with subpool capabilities
   --  for Ada 95, Ada 2005, and Ada 2012. The Deepend storage pools were
   --  designed to provide efficient concurrency for use in both single core
   --  and multicore environments. The Deepend pools also provide flexibility
   --  such that the storage for the pools may be placed entirely in static
   --  memory, on the stack, or on the heap, or in various combinations of the
   --  above (See the Bounded Pools below for more details on how this can
   --  be applied).
   --
   --  Further, Deepend ensures that each pool and subpool is "owned" by a
   --  single Ada task, and thus only that task may allocate objects from a
   --  particular subpool, and only one task may allocate objects directly
   --  to the pool. Ownership of a pool and subpools may be relinquished and
   --  transferred to other tasks. This capability eliminates the need for
   --  expensive locking when allocating and deallocating objects. Attempts
   --  by other tasks to allocate from a pool or subpool owned by another
   --  task results in a Program_Error exception being raised.
   --
   --  Storage pools with subpool capabilities allow all objects in a subpool
   --  to be reclaimed all at once, instead of requiring each object to be
   --  individually reclaimed one at a time. A Dynamic Pool may have any
   --  number of subpools. If subpools are not reclaimed prior to finalization
   --  of the pool, then they are reclaimed when the pool is finalized.
   --
   --  With this Storage pool, Unchecked_Deallocation is implemented as a No-Op
   --  (null procedure), because it is not needed or intended to be used.
   --  If early finalization is needed, Unchecked_Deallocate_Subpool may be
   --  used, which has similar issues as Unchecked_Deallocation, but is
   --  safer, since it can be applied more globally, and less frequently. Even
   --  Unchecked_Deallocate_Subpool is unnecessary for reclaiming subpools in
   --  nested scopes with Deepend, as a scoped subpool facility is also
   --  provided, which automatically finalizes subpools, when leaving
   --  the scope of their declaration.
   --
   --  Subpool based storage management provides a safer means of memory
   --  management, which can outperform other mechanisms for storage
   --  reclamation including garbage collection.
   --
   --  There are 4 Storage Pool packages to choose from in Deepend.
   --
   --     1) Dynamic_Pools
   --     2) Bounded_Dynamic_Pools
   --     3) Basic_Dynamic_Pools
   --     4) Basic_Bounded_Dynamic_Pools
   --
   --  The Dynamic_Pools package has subpool capabilities where the storage
   --  in each Subpool object is unbounded. If the current block of memory
   --  is fully allocated to objects and further objects are allocated,
   --  then another block of storage is allocated to the subpool, and further
   --  allocations to that subpool are carved out of that new storage block.
   --
   --  The Bounded_Dynamic_Pools package has subpool capabilities where the
   --  storage in each Subpool object is bounded, and the number of subpools
   --  that may be allocated is also bounded. If the Subpool is fully
   --  allocated with objects and an attempt is made to allocate further
   --  objects from the same subpool, then a Storage_Error exception is raised.
   --  Similarly, if an attempt is made to allocate more subpools than
   --  the maximum number the pool was configured to manage, then a
   --  Storage_Error exception is raised. A Bounded_Dynamic_Pools pool does
   --  not utilize the heap for its management of subpools. Scoped_Subpool
   --  objects are provided that may be configured to allocate their storage
   --  from the heap, or declared on the stack, or statically at library level.
   --  In particular, Scoped_Subpool objects are included that have
   --  discriminants that provide this control. A scoped subpool automatically
   --  is finalized when execution leaves the scope of the declaration.
   --  For a scoped subpool declared at library level, the
   --  the storage remains available while the partition is active.
   --
   --  The Basic_Dynamic_Pool package does not have subpool capabilities, and
   --  each allocation is managed instead by the pool object. When the pool is
   --  finalized, all objects allocated from the pool that need finalization
   --  are also finalized. A Basic_Dynamic_Pool is an unbounded pool such that
   --  if the current block of storage is fully allocated with objects and
   --  further objects are allocated, then another block of memory is allocated
   --  to the pool, and further object allocations are carved out of that
   --  new block.
   --
   --  The Basic_Bounded_Dynamic_Pool package does not have subpool
   --  capabilities, and each allocation is managed instead by the pool object.
   --  Like the Basic_Dynamic_Pool, when the pool is finalized, all objects
   --  allocated from the pool are also finalized. A Basic_Dynamic_Pool is a
   --  bounded pool such that if the pool's storage has been fully allocated
   --  with objects and an attempt is made to allocate further objects,
   --  then a Storage_Error exception is raised. A Basic_Bounded_Dynamic_Pool
   --  pool has discriminants that indicate whether the storage for the pool
   --  resides on the heap or on the stack, or statically at library level.
   --
   --  Both Basic_Dynamic_Pools and Bounded_Basic_Dynamic_Pools are completely
   --  forward compatible with the Ada 2012 standard for Storage_Pools,
   --  since they only allow allocations via the existing "new" operator
   --  (without subpool specifications). This facility relies on access type
   --  finalization to finalize all the objects from a pool, and does not
   --  otherwise support subpools.
   --
   --  In Ada 2012, the new allocation syntax may be used with this pool, to
   --  specify the subpool that will contain the allocated objects.
   --
   --     e.g.
   --          Object := new (subpool_name) Object_Type'(Value);
   --
   --  For Ada 95 and Ada 2005, a similar effect can be obtained by using the
   --  Allocation and Initialized_Allocation generics provided by this package.
   --  However, these generics only allow allocating non-controlled objects of
   --  definite types to a particular subpool, whereas in Ada 2012, indefinite
   --  types and controlled types, and other types needing finalization such as
   --  protected types may also be allocated to a subpool. Only task types or
   --  types that have tasks cannot be allocated to a subpool.
   --
   --  In addition, for Ada 95, Ada 2005, and Ada 2012, the "new" keyword
   --  may be used without specifying a subpool, which results in an object
   --  being allocated to the default subpool for the storage pool.
   --
   --  A Dynamic_Pool allows objects allocated from a subpool to be reclaimed
   --  all at once, instead of requiring each object to be individually
   --  reclaimed one at a time via the Ada.Unchecked_Deallocation generic.
   --  In fact, Ada.Unchecked_Deallocation is not needed or expected to be used
   --  with this storage pool (and has no effect).
   --
   --  Tasks can create subpools from the same Dynamic Pool object at the
   --  same time, but only one task may allocate objects from a specific
   --  subpool instance at a time. A task "owns" its subpools, and attempts
   --  by other tasks to allocate from subpool owned by another task results in
   --  a Program_Error exception being raised.
   --
   --  There are separate versions of these packages for Ada95, Ada 2005, and
   --  Ada 2012. The three versions were designed to have mostly compatible
   --  interfaces, but there are slight differences in each newer language
   --  version that takes advantages of newer language features.
   --  In particular,
   --    - for Ada 2005, the Scoped_Subpool type has a Create_Subpool
   --      constructor, which allows the Block_Size to have a defaulted value.
   --    - for Ada 2012, Pool parameters are in out parameters, rather than
   --      access parameters, which eliminates the need to declare the pool
   --      object as an aliased object.
   --
   --
   --  Allocation strategy:
   --
   --    Deallocate is not needed or used, and is implemented as a null
   --    procedure. Use of this storage pool means that there is no need for
   --    calls to Ada.Unchecked_Deallocation (as it has no effect).
   --
   --    The strategy is to provide an efficient storage pool that allocates
   --    objects quickly with minimal overhead, and very fast dealloction.
   --    Tasks "own" its subpool objects, which allows allocation from the
   --    subpools to be more efficient, since there is no need for task
   --    synchronization.
   --
   --    The intent is that the subpool strategy should generally outperform
   --    other strategies such as garbage collection, or individual object
   --    reclamation in a more deterministic fashion.
   --
   --  ** NOTE: In the Ada 95 and Ada 2005 version of Dynamic_Pools, it is
   --    erroneous to allocate objects to a subpool that need finalization eg.
   --    (Tasks, protected types, or objects of types inherited from types
   --    defined in Ada.Finalization) and then deallocate the subpool
   --    associated with those objects before they would have otherwise
   --    been finalized.
   --
   --  For Ada 2012, it is only erroneous to allocate task objects or objects
   --  containing task components to a subpool.
   --
   package Basic_Bounded_Dynamic_Pools is

      Default_Size : constant := 16#FFFF#;
      --  A Block Size is the size of the heap allocation used when more
      --  storage is needed for the pool. Larger block sizes imply less
      --  heap allocations. Generally, better performance involves using larger
      --  block sizes.

      type Basic_Dynamic_Pool is new
        Storage_Pools.Root_Storage_Pool with private;
      --  The Size specifies how much storage is managed by the pool.
      --  If Heap_Allocated is true, the storage is allocated from
      --  heap, otherwise the storage is directly in the Pool object.

      function Storage_Size
        (Pool : Basic_Dynamic_Pool) return Storage_Elements.Storage_Count;
      --  Indicates the amount of storage managed by the pool.

      function Storage_Used
        (Pool : Basic_Dynamic_Pool) return Storage_Elements.Storage_Count;
      --  Indicates the current amount of storage allocated to
      --  objects from the pool.

   private

      subtype Storage_Array is System.Storage_Elements.Storage_Array;

      type Storage_Array_Access is access Storage_Array;

      subtype Storage_Array_Index is System.Storage_Elements.Storage_Offset
      range 1 .. System.Storage_Elements.Storage_Offset'Last;

      Size : Storage_Elements.Storage_Count := 8_000_000;

      Active : Storage_Array (1 .. Size);

      Owned : Boolean := False;

      type Basic_Dynamic_Pool is new Storage_Pools.Root_Storage_Pool with
         record
            Next_Allocation : Storage_Array_Index;
            --Owner : Ada.Task_Identification.Task_Id;
         end record;

      use type Storage_Elements.Storage_Count;

      procedure Allocate
        (Pool : in out Basic_Dynamic_Pool;
         Storage_Address : out Address;
         Size_In_Storage_Elements : Storage_Elements.Storage_Count;
         Alignment : Storage_Elements.Storage_Count);
      --  with Pre => Is_Owner (Pool, Current_Task);

      procedure Deallocate
        (Pool : in out Basic_Dynamic_Pool;
         Storage_Address : Address;
         Size_In_Storage_Elements : Storage_Elements.Storage_Count;
         Alignment : Storage_Elements.Storage_Count);

      procedure Initialize (Pool : in out Basic_Dynamic_Pool);

      procedure Finalize   (Pool : in out Basic_Dynamic_Pool);

      --  NOTE: Ada 95 allows multiple subprograms to be mentioned in a single
      --  Inline pragma, but Janus currently doesn't support this, which is why
      --  they are listed separately
      --
      pragma Inline (Allocate);
      pragma Inline (Initialize);
      pragma Inline (Finalize);

   end Basic_Bounded_Dynamic_Pools;

   --  Day 1: Sonar Sweep
   --
   --  You're minding your own business on a ship at sea when the overboard
   --  alarm goes off! You rush to see if you can help. Apparently,
   --  one of the Elves tripped and accidentally sent the sleigh keys
   --  flying into the ocean!
   --
   --  Before you know it, you're inside a submarine the Elves keep ready
   --  for situations like this. It's covered in Christmas lights
   --  (because of course it is), and it even has an experimental antenna
   --  that should be able to track the keys if you can boost its signal
   --  strength high enough; there's a little meter that indicates
   --  the antenna's signal strength by displaying 0-50 stars.
   --
   --  Your instincts tell you that in order to save Christmas, you'll need
   --  to get all fifty stars by December 25th.
   --
   --  Collect stars by solving puzzles. Two puzzles will be made available
   --  on each day in the Advent calendar; the second puzzle is unlocked
   --  when you complete the first. Each puzzle grants one star. Good luck!
   --
   --  As the submarine drops below the surface of the ocean,
   --  it automatically performs a sonar sweep of the nearby sea floor.
   --  On a small screen, the sonar sweep report (your puzzle input)
   --  appears: each line is a measurement of the sea floor depth
   --  as the sweep looks further and further away from the submarine.
   --
   --  For example, suppose you had the following report:
   --
   --  199
   --  200
   --  208
   --  210
   --  200
   --  207
   --  240
   --  269
   --  260
   --  263
   --
   --  This report indicates that, scanning outward from the submarine,
   --  the sonar sweep found depths of 199, 200, 208, 210, and so on.
   --
   --  The first order of business is to figure out how quickly the depth
   --  increases, just so you know what you're dealing with - you never know
   --  if the keys will get carried into deeper water by an ocean current or
   --  a fish or something.
   --
   --  To do this, count the number of times a depth measurement increases
   --  from the previous measurement. (There is no measurement before
   --  the first measurement.) In the example above,
   --  the changes are as follows:
   --
   --  199 (N/A - no previous measurement)
   --  200 (increased)
   --  208 (increased)
   --  210 (increased)
   --  200 (decreased)
   --  207 (increased)
   --  240 (increased)
   --  269 (increased)
   --  260 (decreased)
   --  263 (increased)
   --
   --  In this example, there are 7 measurements that are larger
   --  than the previous measurement.
   --
   --  How many measurements are larger than the previous measurement?
   package Day_1_Part_One is

      procedure Run;

   end Day_1_Part_One;

   --  Part Two
   --
   --  Considering every single measurement isn't as useful as you expected:
   --  there's just too much noise in the data.
   --
   --  Instead, consider sums of a three-measurement sliding window.
   --  Again considering the above example:
   --
   --  199  A
   --  200  A B
   --  208  A B C
   --  210    B C D
   --  200  E   C D
   --  207  E F   D
   --  240  E F G
   --  269    F G H
   --  260      G H
   --  263        H
   --
   --  Start by comparing the first and second three-measurement windows.
   --  The measurements in the first window are marked A (199, 200, 208);
   --  their sum is 199 + 200 + 208 = 607. The second window is
   --  marked B (200, 208, 210); its sum is 618. The sum of measurements
   --  in the second window is larger than the sum of the first,
   --  so this first comparison increased.
   --
   --  Your goal now is to count the number of times the sum of measurements
   --  in this sliding window increases from the previous sum.
   --  So, compare A with B, then compare B with C, then C with D,
   --  and so on. Stop when there aren't enough measurements left to create
   --  a new three-measurement sum.
   --
   --  In the above example, the sum of each three-measurement window is
   --  as follows:
   --
   --  A: 607 (N/A - no previous sum)
   --  B: 618 (increased)
   --  C: 618 (no change)
   --  D: 617 (decreased)
   --  E: 647 (increased)
   --  F: 716 (increased)
   --  G: 769 (increased)
   --  H: 792 (increased)
   --
   --  In this example, there are 5 sums that are larger than
   --  the previous sum.
   --
   --  Consider sums of a three-measurement sliding window.
   --  How many sums are larger than the previous sum?
   package Day_1_Part_Two is

      procedure Run;

   end Day_1_Part_Two;

   --  Day 2: Dive!
   --
   --  Now, you need to figure out how to pilot this thing.
   --
   --  It seems like the submarine can take a series of commands like
   --  forward 1, down 2, or up 3:
   --
   --    forward X increases the horizontal position by X units.
   --    down X increases the depth by X units.
   --    up X decreases the depth by X units.
   --
   --  Note that since you're on a submarine, down and up affect your
   --  depth, and so they have the opposite result of what you
   --  might expect.
   --
   --  The submarine seems to already have a planned course
   --  (your puzzle input). You should probably figure out where
   --  it's going. For example:
   --
   --  forward 5
   --  down 5
   --  forward 8
   --  up 3
   --  down 8
   --  forward 2
   --
   --  Your horizontal position and depth both start at 0.
   --  The steps above would then modify them as follows:
   --
   --  forward 5 adds 5 to your horizontal position, a total of 5.
   --  down 5 adds 5 to your depth, resulting in a value of 5.
   --  forward 8 adds 8 to your horizontal position, a total of 13.
   --  up 3 decreases your depth by 3, resulting in a value of 2.
   --  down 8 adds 8 to your depth, resulting in a value of 10.
   --  forward 2 adds 2 to your horizontal position, a total of 15.
   --
   --  After following these instructions, you would have a horizontal
   --  position of 15 and a depth of 10. (Multiplying these together
   --  produces 150.)
   --
   --  Calculate the horizontal position and depth you would have after
   --  following the planned course. What do you get if you multiply
   --  your final horizontal position by your final depth?
   package Day_2_Part_One is

      procedure Run;

   end Day_2_Part_One;

   --  Based on your calculations, the planned course doesn't seem to make
   --  any sense. You find the submarine manual and discover that
   --  the process is actually slightly more complicated.
   --
   --  In addition to horizontal position and depth, you'll also need
   --  to track a third value, aim, which also starts at 0. The commands
   --  also mean something entirely different than you first thought:
   --
   --    down X increases your aim by X units.
   --    up X decreases your aim by X units.
   --    forward X does two things:
   --        It increases your horizontal position by X units.
   --        It increases your depth by your aim multiplied by X.
   --
   --  Again note that since you're on a submarine,
   --  down and up do the opposite of what you might expect:
   --  "down" means aiming in the positive direction.
   --
   --  Now, the above example does something different:
   --
   --    forward 5 adds 5 to your horizontal position, a total of 5.
   --    Because your aim is 0, your depth does not change.
   --    down 5 adds 5 to your aim, resulting in a value of 5.
   --    forward 8 adds 8 to your horizontal position, a total of 13.
   --    Because your aim is 5, your depth increases by 8*5=40.
   --    up 3 decreases your aim by 3, resulting in a value of 2.
   --    down 8 adds 8 to your aim, resulting in a value of 10.
   --    forward 2 adds 2 to your horizontal position, a total of 15.
   --    Because your aim is 10, your depth increases by 2*10=20
   --    to a total of 60.
   --
   --  After following these new instructions, you would have a horizontal
   --  position of 15 and a depth of 60. (Multiplying these produces 900.)
   --
   --  Using this new interpretation of the commands, calculate
   --  the horizontal position and depth you would have after following
   --  the planned course. What do you get if you multiply your
   --  final horizontal position by your final depth?
   package Day_2_Part_Two is

      procedure Run;

   end Day_2_Part_Two;

   --  Day 3: Binary Diagnostic
   --
   --  The submarine has been making some odd creaking noises,
   --  so you ask it to produce a diagnostic report just in case.
   --
   --  The diagnostic report (your puzzle input) consists of a list
   --  of binary numbers which, when decoded properly, can tell you
   --  many useful things about the conditions of the submarine.
   --  The first parameter to check is the power consumption.
   --
   --  You need to use the binary numbers in the diagnostic report
   --  to generate two new binary numbers (called the gamma rate and
   --  the epsilon rate). The power consumption can then be found
   --  by multiplying the gamma rate by the epsilon rate.
   --
   --  Each bit in the gamma rate can be determined by finding
   --  the most common bit in the corresponding position of all numbers
   --  in the diagnostic report. For example, given the following
   --  diagnostic report:
   --
   --  00100
   --  11110
   --  10110
   --  10111
   --  10101
   --  01111
   --  00111
   --  11100
   --  10000
   --  11001
   --  00010
   --  01010
   --
   --  Considering only the first bit of each number, there are
   --  five 0 bits and seven 1 bits. Since the most common bit is 1,
   --  the first bit of the gamma rate is 1.
   --
   --  The most common second bit of the numbers in the diagnostic report
   --  is 0, so the second bit of the gamma rate is 0.
   --
   --  The most common value of the third, fourth, and fifth bits are
   --  1, 1, and 0, respectively, and so the final three bits of
   --  the gamma rate are 110.
   --
   --  So, the gamma rate is the binary number 10110, or 22 in decimal.
   --
   --  The epsilon rate is calculated in a similar way; rather than use
   --  the most common bit, the least common bit from each position is
   --  used. So, the epsilon rate is 01001, or 9 in decimal.
   --  Multiplying the gamma rate (22) by the epsilon rate (9) produces
   --  the power consumption, 198.
   --
   --  Use the binary numbers in your diagnostic report to calculate
   --  the gamma rate and epsilon rate, then multiply them together.
   --  What is the power consumption of the submarine?
   --  (Be sure to represent your answer in decimal, not binary.)
   package Day_3_Part_One is

      procedure Run;

   end Day_3_Part_One;

   --  Next, you should verify the life support rating, which can be
   --  determined by multiplying the oxygen generator rating by the CO2
   --  scrubber rating.
   --
   --  Both the oxygen generator rating and the CO2 scrubber rating are
   --  values that can be found in your diagnostic report - finding them
   --  is the tricky part. Both values are located using a similar process
   --  that involves filtering out values until only one remains.
   --  Before searching for either rating value, start with the full list
   --  of binary numbers from your diagnostic report and consider just
   --  the first bit of those numbers. Then:
   --
   --  Keep only numbers selected by the bit criteria for the type
   --  of rating value for which you are searching. Discard numbers
   --  which do not match the bit criteria.
   --  If you only have one number left, stop; this is the rating value
   --  for which you are searching.
   --  Otherwise, repeat the process, considering the next bit to the right.
   --
   --  The bit criteria depends on which type of rating value
   --  you want to find:
   --
   --  To find oxygen generator rating, determine the most common value
   --  (0 or 1) in the current bit position, and keep only numbers
   --  with that bit in that position. If 0 and 1 are equally common,
   --  keep values with a 1 in the position being considered.
   --
   --  To find CO2 scrubber rating, determine the least common value
   --  (0 or 1) in the current bit position, and keep only numbers
   --  with that bit in that position. If 0 and 1 are equally common,
   --  keep values with a 0 in the position being considered.
   --
   --  For example, to determine the oxygen generator rating value
   --  using the same example diagnostic report from above:
   --
   --  Start with all 12 numbers and consider only the first bit of
   --  each number. There are more 1 bits (7) than 0 bits (5),
   --  so keep only the 7 numbers with a 1 in the first position:
   --  11110, 10110, 10111, 10101, 11100, 10000, and 11001.
   --  Then, consider the second bit of the 7 remaining numbers:
   --  there are more 0 bits (4) than 1 bits (3), so keep only
   --  the 4 numbers with a 0 in the second position:
   --  10110, 10111, 10101, and 10000.
   --  In the third position, three of the four numbers have a 1,
   --  so keep those three: 10110, 10111, and 10101.
   --  In the fourth position, two of the three numbers have a 1,
   --  so keep those two: 10110 and 10111.
   --  In the fifth position, there are an equal number of
   --  0 bits and 1 bits (one each). So, to find the oxygen generator
   --  rating, keep the number with a 1 in that position: 10111.
   --  As there is only one number left, stop; the oxygen generator rating
   --  is 10111, or 23 in decimal.
   --
   --  Then, to determine the CO2 scrubber rating value from the same
   --  example above:
   --
   --  Start again with all 12 numbers and consider only the first bit of
   --  each number. There are fewer 0 bits (5) than 1 bits (7),
   --  so keep only the 5 numbers with a 0 in the first position:
   --  00100, 01111, 00111, 00010, and 01010.
   --  Then, consider the second bit of the 5 remaining numbers:
   --  there are fewer 1 bits (2) than 0 bits (3), so keep only
   --  the 2 numbers with a 1 in the second position: 01111 and 01010.
   --  In the third position, there are an equal number of 0 bits and
   --  1 bits (one each). So, to find the CO2 scrubber rating,
   --  keep the number with a 0 in that position: 01010.
   --  As there is only one number left, stop; the CO2 scrubber rating
   --  is 01010, or 10 in decimal.
   --
   --  Finally, to find the life support rating, multiply the oxygen
   --  generator rating (23) by the CO2 scrubber rating (10) to get 230.
   --
   --  Use the binary numbers in your diagnostic report to calculate
   --  the oxygen generator rating and CO2 scrubber rating,
   --  then multiply them together. What is the life support rating
   --  of the submarine?
   --  (Be sure to represent your answer in decimal, not binary.)
   package Day_3_Part_Two is

      procedure Run;

   end Day_3_Part_Two;

   --  Day 4: Giant Squid
   --
   --  You're already almost 1.5km (almost a mile) below the surface
   --  of the ocean, already so deep that you can't see any sunlight.
   --  What you can see, however, is a giant squid that has attached
   --  itself to the outside of your submarine.
   --
   --  Maybe it wants to play bingo?
   --
   --  Bingo is played on a set of boards each consisting of a 5x5 grid
   --  of numbers. Numbers are chosen at random, and the chosen number
   --  is marked on all boards on which it appears.
   ---  (Numbers may not appear on all boards.) If all numbers in any row
   --  or any column of a board are marked, that board wins.
   --  (Diagonals don't count.)
   --
   --  The submarine has a bingo subsystem to help passengers (currently,
   --  you and the giant squid) pass the time. It automatically generates
   --  a random order in which to draw numbers and a random set of boards
   --  (your puzzle input). For example:
   --
   -- 7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1
   --
   --  22 13 17 11  0
   --   8  2 23  4 24
   --  21  9 14 16  7
   --   6 10  3 18  5
   --   1 12 20 15 19
   --
   --   3 15  0  2 22
   --   9 18 13 17  5
   --  19  8  7 25 23
   --  20 11 10 24  4
   --  14 21 16 12  6
   --
   --  14 21 17 24  4
   --  10 16 15  9 19
   --  18  8 23 26 20
   --  22 11 13  6  5
   --   2  0 12  3  7
   --
   --  After the first five numbers are drawn (7, 4, 9, 5, and 11),
   --  there are no winners, but the boards are marked as follows
   --  (shown here adjacent to each other to save space):
   --
   --  22 13 17 11  0         3 15  0  2 22        14 21 17 24  4
   --   8  2 23  4 24         9 18 13 17  5        10 16 15  9 19
   --  21  9 14 16  7        19  8  7 25 23        18  8 23 26 20
   --   6 10  3 18  5        20 11 10 24  4        22 11 13  6  5
   --   1 12 20 15 19        14 21 16 12  6         2  0 12  3  7
   --
   --  After the next six numbers are drawn (17, 23, 2, 0, 14, and 21),
   --  there are still no winners:
   --
   --  22 13 17 11  0         3 15  0  2 22        14 21 17 24  4
   --   8  2 23  4 24         9 18 13 17  5        10 16 15  9 19
   --  21  9 14 16  7        19  8  7 25 23        18  8 23 26 20
   --   6 10  3 18  5        20 11 10 24  4        22 11 13  6  5
   --   1 12 20 15 19        14 21 16 12  6         2  0 12  3  7
   --
   --  Finally, 24 is drawn:
   --
   --  22 13 17 11  0         3 15  0  2 22        14 21 17 24  4
   --   8  2 23  4 24         9 18 13 17  5        10 16 15  9 19
   --  21  9 14 16  7        19  8  7 25 23        18  8 23 26 20
   --   6 10  3 18  5        20 11 10 24  4        22 11 13  6  5
   --   1 12 20 15 19        14 21 16 12  6         2  0 12  3  7
   --
   --  At this point, the third board wins because it has at least
   --  one complete row or column of marked numbers (in this case,
   --  the entire top row is marked: 14 21 17 24 4).
   --
   --  The score of the winning board can now be calculated.
   --  Start by finding the sum of all unmarked numbers on that board;
   --  in this case, the sum is 188. Then, multiply that sum by the number
   --  that was just called when the board won, 24, to get the final score,
   --  188 * 24 = 4512.
   --
   --  To guarantee victory against the giant squid, figure out which board
   --  will win first. What will your final score be if you choose
   --  that board?
   package Day_4_Part_One is

      procedure Run;

   end Day_4_Part_One;

end Ac21.Aa;
