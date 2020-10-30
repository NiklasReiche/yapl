# yapl - Yet Another Programming Language 
Just a small personal project for learning purposes.

![Scala CI](https://github.com/NiklasReiche/yapl/workflows/Scala%20CI/badge.svg?branch=master)

#### Project components
The project consists of a lexer, a parser, and an interpreter for a simple programming language.

#### Language properties
- expression-oriented
- prefix notation, e.g. `(+ 1 2)`
- dynamic and strong typing
- eager expression evaluation

#### Current features
- arithmetic, boolean, comparison operations
- global and local variables
- first-class functions and first-class classes
- flow control (sequences, if statements)

### Grammar
See [grammar.md](grammar.md)

### Language Documentation
#### Basic values
The basic values in YAPL are `Num`, which can be any decimal number, and `Bool`, which can be either `true` or `false`.
There is no distinction between integer and floating-point numbers; all `Num` values are double precision floating-point
numbers internally.

#### Built-in operators
The built-in arithmetic and boolean operators are n-ary. Like all expressions they are written in prefix notation.
- Addition: `(+ 1 2 3 4)` evaluates to `10 : Num`
- Subtraction: `(- 1 2 3 4)` evaluates to `-8 : Num`, `(- 2)` evaluates to `-2 : Num`
- Multiplication: `(* 1 2 3 4)` evaluates to `24 : Num`
- Division: `(/ 1 2 3 4)` evaluates to `0.0417 : Num`
- And: `(& true false false)` evaluates to `false : Bool`
- Or: `(| true false true)` evaluates to `true : Bool`
- Not: `(! true)` evaluates to `false : Bool`
- Equality: `(= 1 2)` evaluates to `false : Bool`
- Less than: `(< 4 1)` evaluates to `false : Bool`
- Greater than: `(> 4 1)` evaluates to `true : Bool`

#### Variables
Global variables are defined by `(global <name> <value>)`. The scope of global variables is the entire file. The `global`
construct is only allowed at the top-level of a program.

Local variables are defined in a `let` construct like `(let [x 1] [y 5] [z 3] <body>)`. The scope of these variables
is the `<body>` expression.

The value of a variable can be changed by the `set` operator, e.g. `(set x 42)`

#### Flow control structures
- If-else: `(if <test> <true-clause> <false-clause>)` evaluates the `<true-clause>` if `<test>` evaluates to `true`, else
`<false-clause>` is evaluated.
- Cond: `(cond [<test-expression> <body-expression>] ... [else <body-expression])` evaluates the first `<body-expresison>`
for which the corresponding `<test-expression>` evaluates to `true`. A `<testexpression` declared as `else` always 
evaluates to `true`, which means that the `else` case will be evaluated if no previous cases are `true`. Furthermore, any
cases declared after an `else` case will effectively be ignored.
- Sequences: `(seq <expression> <expression> ...)` evaluates all given expressions. The result of the `seq` operator is
the result of the last given expression.

#### Functions
Functions only exist as values, so in order to define a named function a `func` expression must be assigned to an
identifier using `global` or `let`. Of course, a `func` expression may also be used as an anonymous function without a 
name binding anywhere a function is expected. A function value can be defined by `(func [parameter names] <body>)`.

To call a function use the `call` operator supplying the variable to which the function is bound as well as an argument
for each function parameter, e.g. `(let [f (func [x] (* x x))] (call f [5]))` evaluates to `25 : Num`. 
The `call` keyword, as well as the `[` `]` brackets can also be omitted, simplifying the function call to `(f 5)`.

#### Classes & Objects
Like functions, classes only exist as values. Therefore, a named class can be defined by assigning a `class` expression to an
identifier using `global` or `let`. As with functions a class can also be defined as an anonymous class without a name binding. 
A class consists of fields and methods, which must be specified in the class definition. 
A field is declared as `(field <name>)`, a method as `(method <name> <func-expression>)`.

An object of a class can be instantiated with `(create <class> [field values])`, where `<class>` evaluates to a class value and 
`field values` contains one expression for every defined field in the class. The values are mapped to the fields based
on the field definition order. YAPL provides operators for reading and writing the fields of the resulting object and
for calling methods on the object. `(field-get <object> <field-id>)` reads a field, `(field-set <object> <field-id> <value>)`
writes a field and `(method-call <object> <method-id> [arguments])` calls a method on an object.

Lastly, inside a method body the `this` identifier can be used to refer to a class instance.

A full class definition may for example look as follows:
```
(global Example
    (class
        (field a)
        (field b)
        (method f [x] (+ x (field-get this a)))
    )
)
```

#### Modules
Each file is implicitly a module, which can be imported into another file with the `(import <module-name>)` statement.
The `<module-name>` is the name of the file without the file ending. Relative paths can be used for modules that are not
in the same directory as the importing file.

When a module is imported, all `(global ...)` name bindings of the module are made available to the importing file.

### Standard Library
The standard library is implicitly imported into every file. It includes a `List` class and standard higher order 
functions like `filter` or `map`.

### Examples
- Calculates the factorial of 5:
```
(global factorial 
    (func [n]
        (if (| (= n 1) (= n 0))
            1
            (* n (call factorial (- n 1)))
        )
    )
)

(call factorial [5])
```

- Creates an object of a class and operates on it (evaluates to `8 : Num`):
```
(global Example
    (class
        (field a)
        (method add [x]
            (field-set this a (+ (field-get this a) x))
        )
    )
)

(let [obj (create Example [5])]
    (seq
        (method-call obj add [3])
        (field-get obj a)
    )
)
```
