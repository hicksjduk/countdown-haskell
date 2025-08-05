# countdown-haskell

A solver,
written in Haskell, for the numbers game as played on [the TV show "Countdown"](https://www.channel4.com/programmes/countdown).

## The game

A Countdown numbers game involves building an expression using some source numbers (usually six of them), and the basic arithmetic
operations (addition, subtraction, multiplication and integer division), plus
as many pairs of parentheses as are needed to achieve the desired 
order of evaluation. The objective is to come up with an expression
whose value is as close as possible to the target, which is
a three-digit number in the range 100 to 999 inclusive. It is not necessary
to use all the source numbers, but no number can be used more times than
it occurs among the source numbers.

The source numbers are selected at random from a pool of twenty "small numbers" -
two instances each of the numbers 1 to 10 inclusive - and four "big numbers" -
one instance each of 25, 50, 75 and 100. 

The game can be played by any number of players, although on the standard version of the show there are only two. The procedure for a round of the numbers game is:
* One player specifies how many big numbers should
be selected (between 0 and 4 inclusive).
* Six source numbers are selected, including the specified number of big numbers.
* A target number is randomly generated.
* The players have thirty seconds to come up
with the best answer they can.
* Each player in turn declares the value of the answer they wish to submit.
* Starting with the player(s) whose declaration
is closest to the target, the players' answers are revealed. In order for a player's answer to be valid, their declared value must differ from the target number by not more than ten, and their answer must be a valid expression whose value is equal to their declared value.
* The player or players who submit valid answers that are closest to the target number score points. 10 points are scored for an
answer whose value is exactly equal to the target; 7 for one that differs from
the target by between 1 and 5 inclusive; and 5 for one that differs from the target by between
6 and 10 inclusive. If no player submits a valid answer, nobody scores any points.

## The solver

This solver attempts to find the best possible solution for the given target and
source numbers. What constitutes the best solution employs additional criteria
besides the one used on the show (closer to the target number):

* If two solutions differ from the target number by the same amount, one is considered to
be better if it uses fewer source numbers.
* If two solutions differ from the target number by the same amount and use the same
number of source numbers, one is considered to be better if it requires fewer pairs
of parentheses.

If no expression can be made from the source numbers whose value differs from the
target number by ten or less, no solution is found.

## Building and running the solver

The application is built using [the `stack` tool](https://docs.haskellstack.org/en/stable/README/). Assuming that you have
`stack` installed, you can enter the following commands, with the
root directory of a clone of this repository as the current working
directory:

* `stack build` to build the application.
* `stack test` to build the application and run the unit tests.
* `stack run [args]` to run the application.

## Command-line interface

When running the application, at least one command line argument must be specified.
Every argument must be a number, and the following additional constraints
apply:

* If there is only one argument, this indicates that the target and source numbers should be generated randomly. The argument specifies the number of big source numbers to be used, and must be a number in the range 0 to 4 inclusive.
* If the number of arguments is more than one, this indicates that the source and target numbers are as specified by the arguments. The first specifies the target number,
and must be in the range 100 to 999 inclusive. The remainder (of which there is no requirement for there to be six) specify the source numbers, each of which must conform to the following constraints:
   * Each source number must be either in the range 1 to 10 inclusive, or 25, 50, 75 or 100.
   * A source number less than 25 cannot appear more than twice among the source numbers.
   * A source number greater than 10 cannot appear more than once among the source numbers.

## Code structure

The code of the application follows the standard structure of a `stack` application:

* The main application code is in [the `src` directory](src).
* The code necessary to run the solver from the command line is in [the `app` directory](app).
* The unit tests are in [the `test` directory](test). They depend on [the `hspec` package](https://hspec.github.io/).

## Algorithm structure

The algorithm for solving a Countdown numbers game has the following main steps:

* Make all possible unique permutations of the source numbers, including partial
permutations where not all the numbers are used.
* For each permutation, make all the expressions that can be used by combining the
numbers in the permutation, in the specified order, with the permitted arithmetic operators,
and parentheses as needed to vary the order of evaluation.
* From all the expressions made using all the permutations, find the best solution, if
any solutions exist. This step is performed using a parallel fold, where the list of expressions is split into chunks which are folded separately and in parallel, and the results of folding each chunk are then combined together to find the overall result.

The root of the solver algorithm is the `solve` function, which takes two parameters: an `Int` specifying the target number and a list of `Int` specifying the source numbers. It returns a value of type `Maybe Expression`, which is `Nothing` to denote that no solution could be found, and `Just e` to denote that `e` is the best possible solution.