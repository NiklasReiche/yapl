```
<program> ::=
    <global> |
    <expression> |
    <global> <program> |
    <expression> <program>

<global> ::= '(' 'global' <id> <expression> ')'

<expression> ::=
    <operation>
    <id>
    <number>
    <boolean>

<boolean> ::= 'true' | 'false'

<operation> ::=
    <arithmetic_operator>
    <boolean_operator>
    <comparison_operator>
    <control_structure>
    '(' 'let' ('[' <id> <expression> ']')+ <expression> ')'
    '(' 'func' '[' <id>* ']' <expression> ')'
    '(' 'call' <id> <expression>* ')'
    '(' 'set' <id> <expression> ')'

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