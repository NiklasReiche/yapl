```
<program> ::=
    <global> |
    <expression> |
    <global> <program> |
    <expression> <program>

<global> ::= '(' 'global' <id> <expression> ')'

<expression> ::= <operation> | <value>

<value> ::= <id> | <number> | <boolean> | <function_def> | <class_def>

<boolean> ::= 'true' | 'false'

<function_def> ::= '(' 'func' '[' <id>* ']' <expression> ')'

<class_def> ::= '(' 'class' (<field_def>|<method_def>)* ')'
<field_def> ::= '(' 'field' <id> ')'
<method_def> ::= '(' 'method' <id> '[' <id>* ']' <expression> ')'

<operation> ::=
    <arithmetic_operator>
    <boolean_operator>
    <comparison_operator>
    <control_structure>
    '(' 'let' ('[' <id> <expression> ']')+ <expression> ')'
    '(' 'call' <id> '[' <expression>* ']' ')'
    '(' 'set' <id> <expression> ')'
    '(' 'field-get' <expression> <id> ')'
    '(' 'field-set' <expression> <id> <expression> ')'
    '(' 'method-call' <expression> <id> '[' <expression>* ']' ')'

<arithmetic_operator> ::=
    '(' '+' <expression>+ ')'
    '(' '-' <expression>+ ')'
    '(' '*' <expression> <expression>+ ')'
    '(' '/' <expression> <expression>+ ')'

<boolean_operator> ::=
    '(' '&' <expression>+ ')'
    '(' '|' <expression>+ ')'
    '(' '!' <expression> ')'

<comparison_operator> ::=
    '(' '=' <expression> <expression> ')'
    '(' '<' <expression> <expression> ')'
    '(' '> <expression> <expression> ')'

<control_structure> ::=
    '(' 'if' <expression> <expression> <expression> ')'
    '(' 'seq' <expression>* ')'
```