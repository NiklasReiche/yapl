package myscript.language

sealed abstract class Value

case class VNum(n: Double) extends Value
case class VBool(b: Boolean) extends Value
