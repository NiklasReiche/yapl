package myscript.language

sealed abstract class Expression

case class Num(n: Double) extends Expression

case class Neg(n: Expression) extends Expression

case class Add(lhs: Expression, rhs: Expression) extends Expression
case class Sub(lhs: Expression, rhs: Expression) extends Expression
case class Mul(lhs: Expression, rhs: Expression) extends Expression
case class Div(lhs: Expression, rhs: Expression) extends Expression


case class Bool(b: Boolean) extends Expression

case class Not(b: Expression) extends Expression

case class And(lhs: Expression, rhs: Expression) extends Expression
case class Or(lhs: Expression, rhs: Expression) extends Expression
