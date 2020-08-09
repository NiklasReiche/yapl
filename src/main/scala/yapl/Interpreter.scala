package yapl

import scala.collection.Map
import yapl.language._

class TypeError(val msg: String) extends Exception
class SemanticError(val msg: String) extends Exception

object Interpreter {
    def interpret(expr: Expression, globals: List[Global]): Value = {
        val (globalEnv, store) = interpGlobals(globals)
        interp(expr, globalEnv, store)._1
    }

    type Location = Int
    type Env = Map[Symbol, Location]
    type Store = Map[Location, Value]

    var _currentLocation = 0

    def nextLocation: Location = {
        _currentLocation += 1
        _currentLocation
    }

    private def interpGlobals(globals: List[Global]): (Env, Store) =
        globals.foldLeft((Map.empty: Env, Map.empty: Store))((acc: (Env, Store), global: Global) => {
            var (env, store) = (collection.mutable.Map() ++ acc._1, acc._2)
            val (value, s1) = interp(global.value, env, store)
            val newLocation = nextLocation
            env += global.id.name -> newLocation
            (env, s1 ++ Map(newLocation -> value))
        })

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
            case Let(declarations, body) =>
                val (letEnv, letStore) = interpIdValuePairsRec(declarations, env, env, store)
                interp(body, letEnv, letStore)

            case App(funExpr, args) =>
                interp(funExpr, env, store) match {
                    case (Closure(params, body, fEnv), s1) =>
                        if (params.length != args.length)
                            sys.error(s"Expected ${params.length} arguments, but got ${args.length}")

                        val (funcEnv, funcStore) = interpIdValuePairs(params zip args, env, fEnv, s1)
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
                statements.foldLeft((VoidV(): Value, store: Store))(
                    (acc: (Value, Store), expr: Expression) => interp(expr, env, acc._2)
                )
        }
    }

    /**
     * Creates bindings for multiple identifiers.
     *
     * @param pairs   The pairs of (identifier = value).
     * @param evalEnv The environment in which the vales are evaluated.
     * @param accEnv  The environment to which the new bindings are appended.
     * @param store   The store to use during evaluation and appending.
     * @return A new environment and store containing bindings for each identifier in pairs.
     */
    private def interpIdValuePairs(pairs: List[(Id, Expression)], evalEnv: Env, accEnv: Env, store: Store): (Env, Store) =
        pairs.foldLeft((accEnv, store))((acc, pair) => {
            val (e, s) = acc
            val (id, valueExpr) = pair
            val (value, s1) = interp(valueExpr, evalEnv, s)
            val newLocation = nextLocation
            (e ++ Map(id.name -> newLocation), s1 ++ Map(newLocation -> value))
        })

    /**
     * Creates recursive bindings for multiple identifiers.
     *
     * @param pairs   The pairs of (identifier = value).
     * @param evalEnv The environment in which the vales are evaluated.
     * @param accEnv  The environment to which the new bindings are appended.
     * @param store   The store to use during evaluation and appending.
     * @return A new environment and store containing bindings for each identifier in pairs.
     */
    private def interpIdValuePairsRec(pairs: List[(Id, Expression)], evalEnv: Env, accEnv: Env, store: Store): (Env, Store) =
        pairs.foldLeft((accEnv, store))((acc, pair) => {
            val recEnv = collection.mutable.Map() ++ evalEnv
            val (id, valueExpr) = pair
            val (value, s1) = interp(valueExpr, recEnv, acc._2)
            val newLocation = nextLocation
            recEnv += id.name -> newLocation
            (acc._1 ++ Map(id.name -> newLocation), s1 ++ Map(newLocation -> value))
        })
}
