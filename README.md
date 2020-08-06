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
- arithmetic and boolean operations
- variables
- first class functions (with recursion)
- flow control (sequences, if statements)


### Examples
- Calculates the factorial of 5:
```
(rec factorial 
     (func n
        (if (= n 1)
            1
            (* n (call factorial (- n 1)))
        )
     )
     (call factorial 5)
)
```

### Grammar
```
expression :=
    operation
    Id
    Number
    Boolean

Boolean :=
    true
    false

operation :=
    arithmetic_operator
    boolean_operator
    comparison_operator
    control_structure
    (let Id expression expression)
    (rec Id expression expression)
    (func Id* expression)
    (call Id expression*)
    (set Id expression)

arithmetic_operator :=
    (+ expression+)
    (- expression+)
    (* expression expression+)
    (/ expression expression+)

boolean_operator :=
    (& expression+)
    (| expression+)
    (! expression)

comparison_operator :=
    (= expression expression)
    (< expression expression)
    (> expression expression)

control_structure :=
    (if expression expression expression)
    (seq expression*)
```
