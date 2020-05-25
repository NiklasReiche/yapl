package myscript

import scala.collection.Map
import myscript.language._

object Interpreter {
    def interpret(expr: Expression): Value = interp(expr, Map.empty)

    private type Env = Map[Symbol, Value]

    private def interp(expr: Expression, env: Env): Value = {
        def interpNAryNumOp(operands: List[Expression], op: (Double, Double) => Double, neutralElement: Double): VNum = {
            if (operands.isEmpty) sys.error("Too few operands for built-in operation")
            VNum(operands.foldLeft(neutralElement)((acc, expr) => {
                interp(expr, env) match {
                    case VNum(n) => op(acc, n)
                    case _ => sys.error(s"Type mismatch for built-in operator")
                }
            }))
        }
        def interpNAryBoolOp(operands: List[Expression], op: (Boolean, Boolean) => Boolean, neutralElement: Boolean): VBool = {
            if (operands.isEmpty) sys.error("Too few operands for built-in operation")
            VBool(operands.foldLeft(neutralElement)((acc, expr) => {
                interp(expr, env) match {
                    case VBool(b) => op(acc, b)
                    case _ => sys.error(s"Type mismatch for built-in operator")
                }
            }))
        }

        expr match {
            // Terminal expressions ------------------------------------------------------------------------------------
            case Num(n) => VNum(n)
            case Bool(b) => VBool(b)
            case Id(name) => env(name)

            // Numeric operators ---------------------------------------------------------------------------------------
            case Add(operands) => interpNAryNumOp(operands, _ + _, 0)
            case Mul(operands) => interpNAryNumOp(operands, _ * _, 1)
            case Sub(operands) =>
                if (operands.isEmpty) sys.error("Too few operands for operator <Sub>")
                if (operands.length == 1) interpNAryNumOp(operands, _ - _, 0)
                interp(operands.head, env) match {
                    case VNum(n) => interpNAryNumOp(operands.tail, _ - _, n)
                    case _ => sys.error("Invalid type for built-in operator <Sub>")
                }
            case Div(operands) =>
                if (operands.length < 2) sys.error("Too few operands for operator <Div>")
                interp(operands.head, env) match {
                    case VNum(n) => interpNAryNumOp(operands.tail, _ / _, n)
                    case _ => sys.error("Invalid type for built-in operator <Div>")
                }

            // Boolean operators ---------------------------------------------------------------------------------------
            case And(operands) => interpNAryBoolOp(operands, _ & _, neutralElement = true)
            case Or(operands) => interpNAryBoolOp(operands, _ | _, neutralElement = false)

            case Not(operands) =>
                if (operands.length > 1) sys.error("Too many operands for unary operation <Not>")
                if (operands.isEmpty) sys.error("Too few operands for unary operator <Not>")
                interp(operands.head, env) match {
                    case VBool(b) => VBool(!b)
                    case _ => sys.error(s"Cannot negate ${operands.head}")
                }

            // Special constructs --------------------------------------------------------------------------------------
            case Let(Id(name), valueExpr, body) =>
                interp(body, env ++ Map(name -> interp(valueExpr, env)))
        }
    }
}
