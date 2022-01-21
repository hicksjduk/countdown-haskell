# countdown-haskell

A solver for the numbers game as played on the TV show "Countdown",
written in Haskell.

## Building and running the solver

The application is built using the `stack` tool. Assuming that you have
`stack` installed, you can enter the following commands, with the
root directory of a clone of this repository as the current working
directory:

* `stack build` to build the application
* `stack test` to build the application and run the unit tests
* `stack exec countdown-exe [args]` to run the application.

## Command-line interface

When running the application, at least one command line argument must be specified.
All arguments must be entirely numeric, and the following additional constraints
apply:

* If there is only one argument, it must be a number in the range 0 to 4 inclusive.
This specifies the number of big numbers (greater than 10) that should be included
among the source numbers for a puzzle where the target number and six source numbers
are generated at random.
* If the number of arguments is more than one, the first specifies the target number,
and must be in the range 100 to 999 inclusive. Each of the others specifies a source
number, which must conform to the following constraints:
   * Each source number must be either in the range 1 to 10 inclusive, or 25, 50, 75 or 100.
   * A source number in the range 1 to 10 cannot appear more than twice among the source
     numbers.
   * A source number greater than 10 cannot appear more than once among the source numbers.

## Code structure

The code of the application follows the standard structure of a `stack` application:

* The main application code is in the `src` directory. In this case, it is split into two
files: `Countdown.hs` contains the code specific to the implementation of a Countdown
solver, and `Utils.hs` contains some utility functions are are used by that implementation,
as well as in other parts of the codebase.
* The code necessary to run the solver from the command line is in the `app` directory.
* The unit tests are in the `test` directory. They depend on the `hspec` package.

## Algorithm structure

The algorithm for solving a Countdown numbers game has the following main steps:

* Make all possible unique permutations of the source numbers, including partial
permutations where not all the numbers are used.
* For each permutation, make all the expressions that can be used by combining the
numbers in the permutation, in the specified order, with the permitted arithmetic operators,
and parentheses as needed to vary the order of evaluation.
* From all the expressions made using all the permutations, find the best solution
given the target number. The best solution is defined as the one whose value differs
from the target number by the smallest amount, or if two solutions differ by the same
amount the one that uses fewest source numbers; two solutions that differ from the 
target number by the same amount, and use the same number of source numbers, are
entirely equivalent.