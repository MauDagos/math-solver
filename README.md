# Math Solver

## Description

I ran into a neat math puzzle where, given some numbers in order and given an
expected result, I had to decide which were the mathematic operations to place
between the numbers to reach the result. The rules were:

1. Available operations: addition (`+`), subtraction (`-`), multiplication (`x`)
   & division (`:`)
2. Multiplication and division took precedence.

For example:

``` text
1 [ ] 6 [ ] 2 = 4
```

The operations to reach the result are `+` and `:`, resulting in this solution:

``` text
1 + 6 : 2 = 4
```

After finishing the math puzzle I thought it would be a nice exercise to write a
program which found the solutions, so here it is. Have fun!

## Usage

`math-solver:solve-math` expects a list of integers, as the arguments to the
equation, and another integer, as the result of equation. For example:

``` common-lisp
(math-solver:solve-math '(1 6 2) 4)
;; "1 + 6 : 2"
```

## Important

Tested only on alisp 10.1.
