package myscript

import scala.collection.Map
import myscript.language._

object Interpreter {
    def interpret(expr: Expression): Value = interp(expr, Map.empty, Map.empty)._1

    type Location = Int
    type Env = Map[Symbol, Location]
    type Store = Map[Location, Value]

    var _currentLocation = 0

    def nextLocation: Location = {
        _currentLocation += 1
        _currentLocation
    }

    private def interp(expr: Expression, env: Env, store: Store): (Value, Store) = {
        def interpNAryNumOp(operands: List[Expression],
                            op: (Double, Double) => Double,
                            neutralElement: Double,
                            store: Store):
        (NumV, Store) = {
            if (operands.isEmpty) sys.error("Too few operands for built-in operation")
            val (result, newStore) = operands.
                foldLeft((neutralElement, store))((acc: (Double, Store), expr: Expression) => {
                    val (v, s) = acc
                    interp(expr, env, s) match {
                        case (NumV(n), s2) => (op(v, n), s2)
                        case _ => sys.error(s"Type mismatch for built-in operator")
                    }
                })
            (NumV(result), newStore)
        }
        def interpNAryBoolOp(operands: List[Expression],
                             op: (Boolean, Boolean) => Boolean,
                             neutralElement: Boolean,
                             store: Store):
        (BoolV, Store) = {
            if (operands.isEmpty) sys.error("Too few operands for built-in operation")
            val (result, newStore) = operands.
                foldLeft((neutralElement, store))((acc: (Boolean, Store), expr: Expression) => {
                    val (v, s) = acc
                    interp(expr, env, s) match {
                        case (BoolV(b), s2) => (op(v, b), s2)
                        case _ => sys.error(s"Type mismatch for built-in operator")
                    }
                })
            (BoolV(result), newStore)
        }

        expr match {
            // Terminal expressions ------------------------------------------------------------------------------------
            case Num(n) => (NumV(n), store)
            case Bool(b) => (BoolV(b), store)
            case Fun(params, body) => (Closure(params, body, env), store)
            case Id(name) => (store(env(name)), store)

            // Numeric operators ---------------------------------------------------------------------------------------
            case Add(operands) =>
                interpNAryNumOp(operands, _ + _, 0, store)

            case Mul(operands) =>
                if (operands.length < 2) sys.error("Too few operands for operator <Mul>")
                interpNAryNumOp(operands, _ * _, 1, store)

            case Sub(operands) =>
                if (operands.isEmpty) sys.error("Too few operands for operator <Sub>")
                if (operands.length == 1) interpNAryNumOp(operands, _ - _, 0, store)
                interp(operands.head, env, store) match {
                    case (NumV(n), s1) => interpNAryNumOp(operands.tail, _ - _, n, s1)
                    case _ => sys.error("Invalid type for built-in operator <Sub>")
                }

            case Div(operands) =>
                if (operands.length < 2) sys.error("Too few operands for operator <Div>")
                interp(operands.head, env, store) match {
                    case (NumV(n), s1) => interpNAryNumOp(operands.tail, _ / _, n, s1)
                    case _ => sys.error("Invalid type for built-in operator <Div>")
                }

            // Boolean operators ---------------------------------------------------------------------------------------
            case And(operands) => interpNAryBoolOp(operands, _ & _, neutralElement = true, store)
            case Or(operands) => interpNAryBoolOp(operands, _ | _, neutralElement = false, store)

            case Not(operands) =>
                if (operands.length > 1) sys.error("Too many operands for unary operation <Not>")
                if (operands.isEmpty) sys.error("Too few operands for unary operator <Not>")
                interp(operands.head, env, store) match {
                    case (BoolV(b), s1) => (BoolV(!b), s1)
                    case _ => sys.error(s"Cannot negate ${operands.head}")
                }

            // Comparison operators ------------------------------------------------------------------------------------
            case Equal(operands) =>
                if (operands.length != 2) sys.error("Wrong number of operands for operator =")
                val (op1, s1) = interp(operands.head, env, store)
                val (op2, s2) = interp(operands.tail.head, env, s1)
                (op1, op2) match {
                    case (NumV(a), NumV(b)) => (BoolV(a == b), s2)
                    case (BoolV(a), BoolV(b)) => (BoolV(a == b), s2)
                    case _ => sys.error("Type mismatch for operator =")
                }

            case Less(operands) =>
                if (operands.length != 2) sys.error("Wrong number of operands for operator =")
                val (op1, s1) = interp(operands.head, env, store)
                val (op2, s2) = interp(operands.tail.head, env, s1)
                (op1, op2) match {
                    case (NumV(a), NumV(b)) => (BoolV(a < b), s2)
                    case _ => sys.error("Type mismatch for operator <")
                }

            case Greater(operands) =>
                if (operands.length != 2) sys.error("Wrong number of operands for operator =")
                val (op1, s1) = interp(operands.head, env, store)
                val (op2, s2) = interp(operands.tail.head, env, s1)
                (op1, op2) match {
                    case (NumV(a), NumV(b)) => (BoolV(a > b), s2)
                    case _ => sys.error("Type mismatch for operator >")
                }

            // Special constructs --------------------------------------------------------------------------------------
            case Let(Id(name), valueExpr, body) =>
                val (value, s1) = interp(valueExpr, env, store)
                val newLocation = nextLocation
                interp(body, env ++ Map(name -> newLocation), s1 ++ Map(newLocation -> value))

            case Rec(Id(name), valueExpr, body) =>
                var recEnv = collection.mutable.Map() ++ env
                val (value, s1) = interp(valueExpr, recEnv, store)
                val newLocation = nextLocation
                recEnv += name -> newLocation
                interp(body, recEnv, s1 ++ Map(newLocation -> value))

            case App(funExpr, args) =>
                interp(funExpr, env, store) match {
                    case (Closure(params, body, fEnv), s1) =>
                        if (params.length != args.length)
                            sys.error(s"Expected ${params.length} arguments, but got ${args.length}")

                        val (funcEnv, funcStore) = (params zip args).foldLeft((fEnv, s1))(
                            (acc: (Env, Store), elem: (Id, Expression)) => {
                                val (prevEnv, prevStore) = acc
                                val (param, arg) = elem
                                val (argValue, s2) = interp(arg, env, prevStore)
                                val newLocation = nextLocation
                                (prevEnv ++ Map(param.name -> newLocation), s2 ++ Map(newLocation -> argValue))
                            }
                        )
                        interp(body, funcEnv, funcStore)
                    case _ => sys.error("Can only call functions")
                }

            case Set(Id(name), valueExpr) =>
                val (value, s1) = interp(valueExpr, env, store)
                (value, s1 ++ Map(env(name) -> value))

            // Control structures --------------------------------------------------------------------------------------
            case If(test, ifCase, elseCase) => interp(test, env, store) match {
                case (BoolV(b), s1) =>
                    if (b)
                        interp(ifCase, env, s1)
                    else
                        interp(elseCase, env, s1)
            }

            case Seq(statements) =>
                statements.foldLeft((Void(): Value, store: Store))(
                    (acc: (Value, Store), expr: Expression) => interp(expr, env, acc._2)
                )
        }
    }
}
