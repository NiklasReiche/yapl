package myscript.language

sealed abstract class Expression

case class Num(n: Double) extends Expression
case class Bool(b: Boolean) extends Expression
case class Id(name: Symbol) extends Expression

case class Add(operands: List[Expression]) extends Expression
case class Sub(operands: List[Expression]) extends Expression
case class Mul(operands: List[Expression]) extends Expression
case class Div(operands: List[Expression]) extends Expression

case class Not(operands: List[Expression]) extends Expression
case class And(operands: List[Expression]) extends Expression
case class Or(operands: List[Expression]) extends Expression

case class Let(name: Id, value: Expression, body: Expression) extends Expression