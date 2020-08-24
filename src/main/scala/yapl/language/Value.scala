package yapl.language

import yapl.Interpreter.Env
import yapl.Interpreter.Location

sealed abstract class Value

case class VoidV() extends Value
case class ErrorV() extends Value
case class NumV(n: Double) extends Value
case class BoolV(b: Boolean) extends Value
case class Closure(params: List[Id], body: Expression, env: Env) extends Value
case class ClassV(fields: List[Id], methods: Map[Id, Closure]) extends Value
case class Object(c: ClassV, fieldValues: List[Location]) extends Value
