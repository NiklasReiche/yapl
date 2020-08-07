package myscript.language

import myscript.Interpreter.Env

sealed abstract class Value

case class VoidV() extends Value
case class NumV(n: Double) extends Value
case class BoolV(b: Boolean) extends Value
case class Closure(params: List[Id], body: Expression, env: Env) extends Value
