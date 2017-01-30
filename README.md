sudoku
======

My personal sudoku solver implementation using
[OCaml](http://caml.inria.fr/) 
(released under MIT licence).

The code and outputs are in french, sorry about that!

Description
-----------

I wrote this sudoku solver in December 2007 to see if my personal "manual" 
algorithm
to solve sudokus was enough to solve the sudokus provided by the
newspapers. Since I was too lazy to do it by hand, I implemented my
algorithm in OCaml since the computer is much faster than I can be :)

### Subsets 

My algorithm is as follow: suppose that you have a line like this

    |1| | |5| | |6| | |

then I know that the number in every other cell of the line must lie in the
set

    {2,3,4,7,8,9}

By taking into account the constraints also coming from columns and
sub-grids, we might get some constraints informations like this in the
line:

    |1|{2,3,4}|{2,7,8,9}|5|{3,4}|{4,7,8}|6|{8,9}|{2,3}|

Now look at the union of the constraints on cells `2`, `4` and `9`, we only
have the 3 possibilities `{2,3,4}`. But since this is for only 3 cells, we
know that the values `2`, `3` and `4` can only appear on these cells. So we
may update the constraints of the other cells:

    |1|{2,3,4}|{7,8,9}|5|{3,4}|{7,8}|6|{8,9}|{2,3}|

More generally, if the union of the constraints of n cells in the line is a
set of cardinality n, then we know that the values of every other cells in
the line cannot be in this set. (If the set if of cardinality strictly
smaller than n, then of course the Sudoku is not resolube!)

This generalize the simplest sudoku solving methods called naked singles
(which corresponds to n=1) and hidden singles (which corresponds to n=8).
This technique seems to be known under the name naked/hidden subsets.

### Forced cells

Another "manual" algorithm that I use, is what I call forced cells: suppose
that the constraints of the first line look like this

    |3|{2,4}|5|{1,6,7,9}|{3,4,7}|{7,8,9}|{2,6}|{8,9}|{6,8,9}|

then you can see that the value `7` can only appear in the top middle
block. Therefore, you know that in the top middle block the value `7` is in
the first line. Therefore you can remove `7` in the second and third lines
of the top middle block.

This give relations between blocks and lines, and between blocks and
columns. Unlike the previous algorithm based on subsets, this work only for
the specific topology of the standard sudokus, while the implementation can
be used for more general sudokus. Therefore you can switch it off by
passing the `-nf` option.

### Backtracking

Now, sometime these two algorithms are not enough to solve a sudoku. In
this case we have to use backtracking: we guess the value of a cell, and
try to solve the sudoku (eventually using backtracking again: when we have
guessed n values, I will say we are in backtracking depth n). We iterate
through all possible values of the given cell. 

Computers are very good at backtracking, and in fact the simplest solver
that you can write is one that only do backtracking, and only checks when
there is an obvious contradiction to the sudoku rules. I have implemented
one in the code: `complete_sudoku`, you can see how short it is in the
source code.

But humans are very bad at handling backtracking. A more human way of doing
something similar to backtracking would be to make a guess of a value, and
use it to update the list of constraints. An easy example is if the value
chosen lead to no solutions when using the above two algorithms (subsets
and forced cells), then we know that this value is not possible, and we can
update the constraints by removing the value. If this is still not enough
to solve the sudoku using only one guess at a time, we will allow the
program to make another guess after the first one to see if the sudoku has
a solution or not, and so on... (I will call that the recursive depth to
distinguish it from the backtracking depth, it will be of level n when the
program can make up to n guess).

A more efficient way to update the list of constraints is, rather than
using the above two algorithms to check if the sudoku has no solution, is
we use them and see what new constraint list we get for the value we have
guessed. We do that with every possible in the cell, and then we know that
the constraint list has to be in the union of all possible constraints list 
for each possible values of the cell.

Maybe an example will be more clear: suppose that the constraint list for
cell A is `{1,3}`, and the one for cell B is `{4,5,6,7}`. Now suppose that
when we fix A to be `1`, we get `{4,5}` for the updated constraint list in
B, and when we fix A to be `3` we get `{5,7}` for B. Then we know that the
constraint list for B has to be `{4,5,7}`.

Virtually all sudoku solving methods described in sudoku sites are
specific examples of subsets and forced cells with a recursive depth of 1. 
I will call a sudoku to be of depth n if it requires the algorithm to go up
to recursive depth n to solve it. One can assume that depth 2 sudokus would
be extremely hard to solve by humans, while sudokus of depth 0 should be
relatively easy.

Most of the examples coming from newspapers I tested were of depth 0, but
some were of depth 1. Generating 10000 random sudokus, around 1/3 were of
depth 0 while the rest of depth 1. Depth 2 Sudokus are very rare, but they
do exist. Check ex-sudos for some examples. I don't know of any sudoku of
depth 3.

Implementation
--------------

The OCaml implementation use what I call iterators: they are OCaml objects 
that have a `begin`, `next`, `out_of_bounds` and `category` methods.
Category can be either a `block`, `line` or `column` and is mostly used for
verbose output. I use the iterators to iterate through all lines, columns
and blocks of the sudoku, and also to iterate through all cells inside a
line, column or block.

This completly generic implementation makes it very easy to modify the code
to solve sudokus which are not `9x9` cells, or even sudokus of dimension
greater than 2, or even sudoku with satisfy other constraints than the ones
for lines, columns and blocks.

Usage
-----

    ocaml sudoku.ml sudoku.txt

`sudoku.txt` should contain a list of numbers (one by cell), where `0` or `.`
represent the empty cell. Every other character is ignored, which allow to
put `sudoku.txt` in a nice ascii-art format (see the examples in ex-sudos to
see some sudokus you can input to the program). If you pass no arguments,
then sudoku.ml will listen to the standard input.

The interpreter can be quite slow with hard to solve sodokus that require
backtracking, so you can also compile sudoku.ml using the bytecode `ocamlc`
compiler or the native code `ocamlopt` compiler. Consider adding the
`-unsafe` option to turn off array accesses checks, since the `sudoku.ml`
code use a lot of these, this option speeds it up a lot.

    ocamlopt -unsafe -o sudoku sudoku.ml
    ./sudoku sudoku.txt

Options
-------

### Customizing the solving algorithm

`-b`
: Set the backtracking method
  - 0: don't do recursion or backtrack
  - 1: don't do recursion, only backtrack
  - 2: do recursions. Backtracking will then not be needed except when the
    sudoku has multiple solutions (and hence is not a true solution). In
    this case `-b 1` will print the solutions much faster because a
    recursion is a lot more intensive than a backtrack
  - 3: do recursions, but once at level n, handle all cells at this level
    rather than going back to level 0 as soon as we have found a new
    constraints (this will usually be faster, but a human solving the
    sudoku would usually work like `-b 2` than `-b 3`).
  - 4: like `-b 2`, but store the constraints found during the recursion
    for a time/memory trade-off (which will only happen when the algorithm
    need a depth 2).
  - 5: like `-b 3`, but store the constraints found during the recursion
    for a time/memory trade-off (which will only happen when the algorithm
    need a depth 2).
  - Add 10 if during recursion, you do not want to use the full recursion
    power described in the algorithm (taking the union of the constraints
    found), but only to check if a value give a non solvable sudoku.
  - Add 400 if you want only to do recursions, not backtracking (such if a
    sudoku has multiple solutions, they won't be found). Note that `-b 401`
    will be similar to `-b 0`.
  Redondant avec les resultats intermediaires, surtout si on backtracke tout de suite
  Rajouter 400 pour ne pas backtracker une fois qu'on a fini les essais. -b 401 est similaire à -b 0...";

`-f`/`-nf`
:Use/Don't use forced cells.

`-bo` min/max/no
:Which cell to use for recursion/backtracking (min number of constraints, most number of constraints, the first one we find).

`-o`/`-no`
: Count/Don't count the number of operations

By defaut the program output the backtracking and recursive depth, the number of
iteration (one iteration consist of applying the subsets and forced cells
algorithm to every iterators), and the number of operations (how many
subsets of length n were used, and how many forced cells).

### Output

`-ab`/`-nab`
: Print/Don't print extra informations during backtrack.
  (You can get more info by adding 100 to -b to print the best looking result 
  before backtracking, 200 to print the best looking constraints before 
  recursion, and 300 for both to print both. A bit redondant if `-ai` is
  also enabled).

`-ad`/`-nad`
: Print/Don't print extra informations during recursion

`-ar`/`-nar`
: Print/Don't print intermediate solutions found during backtracking

`-ari`/`-nari`
: Print/Don't print intermediate solutions found during recursion

`-ai`/`-nai`
: Print/Don't print the current looking sudoku at each step

`-ac`/`-nac`
: Print/Don't print the current constraints at each step

`-ao`/`-nao`
: Print/Don't print the number of opeations during each step (if `-no`, only print the number of iterations made)

`-i`
: Equivalent to `-ai -ad -ao -ari`

`-ni`
: Equivalent to `-nai -nad -nao -nari`

`-quiet`
: Only print the solution 
  (Activates `-ni -nar -nac -nab` and suppress some informative messages)

The defaut command line is  `-b 2 -f -bo min -o -ar -ab -ni`.

## Copyright

Copyright © 2007–2016 Damien Robert

MIT License. See {file:COPYING} for more details.
