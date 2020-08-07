# yapl - Yet Another Programming Language
Just a small personal project for learning purposes.

#### Project components
The project consists of a lexer, a parser, and an interpreter for a simple programming language.

#### Language description
- prefix notation, e.g. ```(+ 1 2)```
- dynamic and strong typing
- eager expression evaluation
- n-ary default operators ```+, -, *, /, &, |``` 

### Current features
- arithmetic, boolean, comparison operations
- variables
- first class functions (with recursion)
- flow control (sequences, if statements)

### Language Components
- Expressions always start with ```(``` followed by the operator and parameters, and end with ```)```.
For example, `(+ 3 6)` adds two numbers. The basic arithmetic and boolean operators are n-ary, so 
adding more numbers simply becomes `(+ 2 7 3 1 3)`.
- Variables can be global or local. Global variables are defined by `(global name value)`, while local
variables are used in `let` expressions, e.g. `(let [x 5] [y (+ 4 2)] (+ x y))`. While only one variable
can be defined per `global` statement, multiple variables can be specified in a `let` expression. 
The scope of local variables is the expression which is supplied as the last parameter to the `let` operator. 
Variables can be reassigned with the `set` operator.
- Functions only exist as values, so in order to define a function a `func` expression must be assigned to an
identifier using `global` or `let`. The `func` operator takes a list of parameter names and a function body expression.
To call a function use the `call` operator supplying the variable to which the function is bound as well as an argument
for each function parameter.

### Examples
- Calculates the factorial of 5:
```
(global factorial 
    (func [n]
        (if (= n 1)
            1
            (* n (call factorial (- n 1)))
        )
    )
)

(call factorial 5)
```

### Grammar
See [grammar.md](grammar.md)
