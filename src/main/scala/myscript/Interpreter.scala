package myscript

import scala.collection.Map
import myscript.language._

object Interpreter {
    def interpret(expr: Expression): Value = interp(expr, Map.empty)

    type Env = Map[Symbol, Value]

    private def interp(expr: Expression, env: Env): Value = {
        def interpNAryNumOp(operands: List[Expression], op: (Double, Double) => Double, neutralElement: Double): NumV = {
            if (operands.isEmpty) sys.error("Too few operands for built-in operation")
            NumV(operands.foldLeft(neutralElement)((acc, expr) => {
                interp(expr, env) match {
                    case NumV(n) => op(acc, n)
                    case _ => sys.error(s"Type mismatch for built-in operator")
                }
            }))
        }
        def interpNAryBoolOp(operands: List[Expression], op: (Boolean, Boolean) => Boolean, neutralElement: Boolean): BoolV = {
            if (operands.isEmpty) sys.error("Too few operands for built-in operation")
            BoolV(operands.foldLeft(neutralElement)((acc, expr) => {
                interp(expr, env) match {
                    case BoolV(b) => op(acc, b)
                    case _ => sys.error(s"Type mismatch for built-in operator")
                }
            }))
        }

        expr match {
            // Terminal expressions ------------------------------------------------------------------------------------
            case Num(n) => NumV(n)
            case Bool(b) => BoolV(b)
            case Id(name) => env(name)

            // Numeric operators ---------------------------------------------------------------------------------------
            case Add(operands) =>
                interpNAryNumOp(operands, _ + _, 0)

            case Mul(operands) =>
                if (operands.length < 2) sys.error("Too few operands for operator <Mul>")
                interpNAryNumOp(operands, _ * _, 1)

            case Sub(operands) =>
                if (operands.isEmpty) sys.error("Too few operands for operator <Sub>")
                if (operands.length == 1) interpNAryNumOp(operands, _ - _, 0)
                interp(operands.head, env) match {
                    case NumV(n) => interpNAryNumOp(operands.tail, _ - _, n)
                    case _ => sys.error("Invalid type for built-in operator <Sub>")
                }

            case Div(operands) =>
                if (operands.length < 2) sys.error("Too few operands for operator <Div>")
                interp(operands.head, env) match {
                    case NumV(n) => interpNAryNumOp(operands.tail, _ / _, n)
                    case _ => sys.error("Invalid type for built-in operator <Div>")
                }

            // Boolean operators ---------------------------------------------------------------------------------------
            case And(operands) => interpNAryBoolOp(operands, _ & _, neutralElement = true)
            case Or(operands) => interpNAryBoolOp(operands, _ | _, neutralElement = false)

            case Not(operands) =>
                if (operands.length > 1) sys.error("Too many operands for unary operation <Not>")
                if (operands.isEmpty) sys.error("Too few operands for unary operator <Not>")
                interp(operands.head, env) match {
                    case BoolV(b) => BoolV(!b)
                    case _ => sys.error(s"Cannot negate ${operands.head}")
                }

            // Special constructs --------------------------------------------------------------------------------------
            case Let(Id(name), valueExpr, body) =>
                interp(body, env ++ Map(name -> interp(valueExpr, env)))

            case Fun(params, body) =>
                Closure(params, body, env)

            case App(funExpr, args) => interp(funExpr, env) match {
                case Closure(params, body, env) =>
                    if (params.length != args.length) sys.error(s"Expected ${params.length} arguments, but got ${args.length}")
                    val funEnv = (params zip args).foldLeft(env)(
                        (e: Env, pair: (Id, Expression)) => e ++ Map(pair._1.name -> interp(pair._2, e))
                    )
                    interp(body, funEnv)
                case _ => sys.error("Can only call functions")
            }
        }
    }
}
