package myscript.language

sealed class Global(val id: Id, val value: Expression)
sealed abstract class Expression

case class Void() extends Expression

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

case class Equal(operands: List[Expression]) extends Expression
case class Greater(operands: List[Expression]) extends Expression
case class Less(operands: List[Expression]) extends Expression

case class Let(declarations: List[(Id, Expression)], body: Expression) extends Expression

case class Set(name: Id, value: Expression) extends Expression

case class Fun(params: List[Id], body: Expression) extends Expression
case class App(fun: Expression, args: List[Expression]) extends Expression

case class If(test: Expression, ifCase: Expression, elseCase: Expression) extends Expression
case class Seq(statements: List[Expression]) extends Expression
