with System.Storage_Elements; use System;
with System.Storage_Pools;

private package Ac22.Aa is

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

   --  Day 1: Calorie Counting
   --
   --  Santa's reindeer typically eat regular reindeer food, but they need
   --  a lot of magical energy to deliver presents on Christmas. For that,
   --  their favorite snack is a special type of star fruit that only grows
   --  deep in the jungle. The Elves have brought you on their
   --  annual expedition to the grove where the fruit grows.
   --
   --  To supply enough magical energy, the expedition needs to retrieve
   --  a minimum of fifty stars by December 25th. Although the Elves assure
   --  you that the grove has plenty of fruit, you decide to grab any fruit
   --  you see along the way, just in case.
   --
   --  Collect stars by solving puzzles. Two puzzles will be made available
   --  on each day in the Advent calendar; the second puzzle is unlocked
   --  when you complete the first. Each puzzle grants one star. Good luck!
   --
   --  The jungle must be too overgrown and difficult to navigate in vehicles
   --  or access from the air; the Elves' expedition traditionally
   --  goes on foot. As your boats approach land, the Elves begin taking
   --  inventory of their supplies. One important consideration is
   --  food - in particular, the number of Calories each Elf is carrying
   --  (your puzzle input).
   --
   --  The Elves take turns writing down the number of Calories contained
   --  by the various meals, snacks, rations, etc. that they've brought
   --  with them, one item per line. Each Elf separates their own inventory
   --  from the previous Elf's inventory (if any) by a blank line.
   --
   --  For example, suppose the Elves finish writing their items' Calories
   --  and end up with the following list:
   --
   --  1000
   --  2000
   --  3000
   --
   --  4000
   --
   --  5000
   --  6000
   --
   --  7000
   --  8000
   --  9000
   --
   --  10000
   --
   --  This list represents the Calories of the food carried by five Elves:
   --
   --      The first Elf is carrying food with 1000, 2000, and 3000 Calories,
   --        a total of 6000 Calories.
   --      The second Elf is carrying one food item with 4000 Calories.
   --      The third Elf is carrying food with 5000 and 6000 Calories,
   --        a total of 11000 Calories.
   --      The fourth Elf is carrying food with 7000, 8000, and 9000 Calories,
   --        a total of 24000 Calories.
   --      The fifth Elf is carrying one food item with 10000 Calories.
   --
   --  In case the Elves get hungry and need extra snacks, they need to know
   --  which Elf to ask: they'd like to know how many Calories are being
   --  carried by the Elf carrying the most Calories. In the example above,
   --  this is 24000 (carried by the fourth Elf).
   --
   --  Find the Elf carrying the most Calories. How many total Calories
   --  is that Elf carrying?
   package Day_1_Part_One is

      procedure Run;

   end Day_1_Part_One;

   --  Part Two
   --
   --  By the time you calculate the answer to the Elves' question,
   --  they've already realized that the Elf carrying the most
   --  Calories of food might eventually run out of snacks.
   --
   --  To avoid this unacceptable situation, the Elves would instead
   --  like to know the total Calories carried by the top three Elves
   --  carrying the most Calories. That way, even if one of those Elves
   --  runs out of snacks, they still have two backups.
   --
   --  In the example above, the top three Elves are the fourth Elf
   --  (with 24000 Calories), then the third Elf (with 11000 Calories),
   --  then the fifth Elf (with 10000 Calories).
   --  The sum of the Calories carried by these three elves is 45000.
   --
   --  Find the top three Elves carrying the most Calories.
   --  How many Calories are those Elves carrying in total?
   package Day_1_Part_Two is

      procedure Run;

   end Day_1_Part_Two;

   --  Day 2: Rock Paper Scissors
   --
   --  The Elves begin to set up camp on the beach. To decide whose tent gets
   --  to be closest to the snack storage, a giant Rock Paper Scissors
   --  tournament is already in progress.
   --
   --  Rock Paper Scissors is a game between two players. Each game contains
   --  many rounds; in each round, the players each simultaneously choose
   --  one of Rock, Paper, or Scissors using a hand shape. Then, a winner
   --  for that round is selected: Rock defeats Scissors,
   --  Scissors defeats Paper, and Paper defeats Rock. If both players choose
   --  the same shape, the round instead ends in a draw.
   --
   --  Appreciative of your help yesterday, one Elf gives you an encrypted
   --  strategy guide (your puzzle input) that they say will be sure
   --  to help you win. "The first column is what your opponent is going
   --  to play: A for Rock, B for Paper, and C for Scissors. The second
   --  column--" Suddenly, the Elf is called away to help with someone's tent.
   --
   --  The second column, you reason, must be what you should play in response:
   --  X for Rock, Y for Paper, and Z for Scissors. Winning every time would
   --  be suspicious, so the responses must have been carefully chosen.
   --
   --  The winner of the whole tournament is the player with the highest score.
   --  Your total score is the sum of your scores for each round. The score
   --  for a single round is the score for the shape you selected
   --  (1 for Rock, 2 for Paper, and 3 for Scissors) plus the score for
   --  the outcome of the round (0 if you lost, 3 if the round was a draw,
   --  and 6 if you won).
   --
   --  Since you can't be sure if the Elf is trying to help you or trick you,
   --  you should calculate the score you would get if you were to follow
   --  the strategy guide.
   --
   --  For example, suppose you were given the following strategy guide:
   --
   --  A Y
   --  B X
   --  C Z
   --
   --  This strategy guide predicts and recommends the following:
   --
   --      In the first round, your opponent will choose Rock (A),
   --        and you should choose Paper (Y). This ends in a win for you with
   --        a score of 8 (2 because you chose Paper + 6 because you won).
   --      In the second round, your opponent will choose Paper (B), and you
   --        should choose Rock (X). This ends in a loss for you with a score
   --        of 1 (1 + 0).
   --      The third round is a draw with both players choosing Scissors,
   --        giving you a score of 3 + 3 = 6.
   --
   --  In this example, if you were to follow the strategy guide, you would
   --  get a total score of 15 (8 + 1 + 6).
   --
   --  What would your total score be if everything goes exactly according
   --  to your strategy guide?
   package Day_2_Part_One is

      procedure Run;

   end Day_2_Part_One;

   --  Part Two
   --
   --  The Elf finishes helping with the tent and sneaks back over to you.
   --  "Anyway, the second column says how the round needs to end:
   --  X means you need to lose, Y means you need to end the round in a draw,
   --  and Z means you need to win. Good luck!"
   --
   --  The total score is still calculated in the same way, but now you need
   --  to figure out what shape to choose so the round ends as indicated.
   --  The example above now goes like this:
   --
   --    In the first round, your opponent will choose Rock (A),
   --    and you need the round to end in a draw (Y), so you also choose Rock.
   --    This gives you a score of 1 + 3 = 4.
   --    In the second round, your opponent will choose Paper (B),
   --    and you choose Rock so you lose (X) with a score of 1 + 0 = 1.
   --    In the third round, you will defeat your opponent's Scissors with Rock
   --    for a score of 1 + 6 = 7.
   --
   --  Now that you're correctly decrypting the ultra top secret strategy
   --  guide, you would get a total score of 12.
   --
   --  Following the Elf's instructions for the second column, what would
   --  your total score be if everything goes exactly according to your
   --  strategy guide?
   package Day_2_Part_Two is

      procedure Run;

   end Day_2_Part_Two;

   --  Day 3: Rucksack Reorganization
   --
   --  One Elf has the important job of loading all of the rucksacks
   --  with supplies for the jungle journey. Unfortunately, that Elf didn't
   --  quite follow the packing instructions, and so a few items now need
   --  to be rearranged.
   --
   --  Each rucksack has two large compartments. All items of a given type
   --  are meant to go into exactly one of the two compartments. The Elf that
   --  did the packing failed to follow this rule for exactly
   --  one item type per rucksack.
   --
   --  The Elves have made a list of all of the items currently
   --  in each rucksack (your puzzle input), but they need your help finding
   --  the errors. Every item type is identified by a single lowercase or
   --  uppercase letter (that is, a and A refer to different types of items).
   --
   --  The list of items for each rucksack is given as characters
   --  all on a single line. A given rucksack always has the same
   --  number of items in each of its two compartments, so the first
   --  half of the characters represent items in the first compartment,
   --  while the second half of the characters represent items
   --  in the second compartment.
   --
   --  For example, suppose you have the following list of contents
   --  from six rucksacks:
   --
   --  vJrwpWtwJgWrhcsFMMfFFhFp
   --  jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
   --  PmmdzqPrVvPwwTWBwg
   --  wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
   --  ttgJtRGJQctTZtZT
   --  CrZsJsPPZsGzwwsLwLmpwMDw
   --
   --    - The first rucksack contains the items vJrwpWtwJgWrhcsFMMfFFhFp,
   --        which means its first compartment contains the items vJrwpWtwJgWr,
   --        while the second compartment contains the items hcsFMMfFFhFp.
   --        The only item type that appears in both compartments
   --        is lowercase p.
   --    - The second rucksack's compartments contain jqHRNqRjqzjGDLGL and
   --        rsFMfFZSrLrFZsSL. The only item type that appears in both
   --        compartments is uppercase L.
   --    - The third rucksack's compartments contain PmmdzqPrV and vPwwTWBwg;
   --        the only common item type is uppercase P.
   --    - The fourth rucksack's compartments only share item type v.
   --    - The fifth rucksack's compartments only share item type t.
   --    - The sixth rucksack's compartments only share item type s.
   --
   --  To help prioritize item rearrangement, every item type can be converted
   --  to a priority:
   --
   --    - Lowercase item types a through z have priorities 1 through 26.
   --    - Uppercase item types A through Z have priorities 27 through 52.
   --
   --  In the above example, the priority of the item type that appears
   --  in both compartments of each rucksack is 16 (p), 38 (L), 42 (P),
   --  22 (v), 20 (t), and 19 (s); the sum of these is 157.
   --
   --  Find the item type that appears in both compartments of each rucksack.
   --  What is the sum of the priorities of those item types?
   package Day_3_Part_One is

      procedure Run;

   end Day_3_Part_One;

   --  Part Two
   --
   --  As you finish identifying the misplaced items, the Elves come to you
   --  with another issue.
   --
   --  For safety, the Elves are divided into groups of three. Every Elf
   --  carries a badge that identifies their group. For efficiency,
   --  within each group of three Elves, the badge is the only item type
   --  carried by all three Elves. That is, if a group's badge is item type B,
   --  then all three Elves will have item type B somewhere in their rucksack,
   --  and at most two of the Elves will be carrying any other item type.
   --
   --  The problem is that someone forgot to put this year's updated
   --  authenticity sticker on the badges. All of the badges need to be pulled
   --  out of the rucksacks so the new authenticity stickers can be attached.
   --
   --  Additionally, nobody wrote down which item type corresponds to each
   --  group's badges. The only way to tell which item type is the right one
   --  is by finding the one item type that is common between all three Elves
   --  in each group.
   --
   --  Every set of three lines in your list corresponds to a single group,
   --  but each group can have a different badge item type. So, in the above
   --  example, the first group's rucksacks are the first three lines:
   --
   --    vJrwpWtwJgWrhcsFMMfFFhFp
   --    jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
   --    PmmdzqPrVvPwwTWBwg
   --
   --  And the second group's rucksacks are the next three lines:
   --
   --    wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
   --    ttgJtRGJQctTZtZT
   --    CrZsJsPPZsGzwwsLwLmpwMDw
   --
   --  In the first group, the only item type that appears in all three
   --  rucksacks is lowercase r; this must be their badges. In the second
   --  group, their badge item type must be Z.
   --
   --  Priorities for these items must still be found to organize the sticker
   --  attachment efforts: here, they are 18 (r) for the first group
   --  and 52 (Z) for the second group. The sum of these is 70.
   --
   --  Find the item type that corresponds to the badges of each three-Elf
   --  group. What is the sum of the priorities of those item types?
   package Day_3_Part_Two is

      procedure Run;

   end Day_3_Part_Two;

end Ac22.Aa;
