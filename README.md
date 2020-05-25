# myscript - A simple programming language

Just a small personal project for learning purposes.

#### Project components
The project consists of a lexer, a parser, and an interpreter for a simple functional programming language.

#### Language description
- prefix notation for operators and function calls, e.g. ```(+ 1 2)``` or ```(func 42 a b)```
- dynamic and strong typing
- eager evaluation of expressions
- first class functions
- default operators ```+, -, *, /, &, |``` are n-ary, e.g. ```(+ 1 2 3)``` evaluates to 6
- unary operators ```-, !```, e.g. ```(- 3)```
- expressions can be simplified by defining constants, e.g. ```(let a (+ 2 5) (* 2 a))``` where a=7 and the result is 2*a=14

### Current functionalities
- arithmetic operations
- named constants

### Planned
- boolean operations
- functions
- recursion
- sequence of expressions
- flow control structures (if, switch, loops)
- mutable state
- objects

### Examples
- Arithmetic operation using a named constant. Calculates ```(x-1)*42``` where ```x=12```
```
(let x (+ 2 4 6) (
  (* (- x 1) 42)
))
```
