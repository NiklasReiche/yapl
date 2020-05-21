package myscript.language

sealed abstract class Type

case class TNum() extends Type
case class TBool() extends Type
