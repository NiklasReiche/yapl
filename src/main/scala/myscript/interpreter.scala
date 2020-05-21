package myscript

import scala.collection.Map
import myscript.language._

object interpreter {

    type Env = Map[Symbol, Value]

    private sealed trait BinOp
    private case object OpAdd extends BinOp
    private case object OpSub extends BinOp
    private case object OpMul extends BinOp
    private case object OpDiv extends BinOp
    private case object OpAnd extends BinOp
    private case object OpOr extends BinOp

    private def interpBinOp(lhs: Expression, rhs: Expression, op: BinOp, env: Env): Value = {
        val lhsV = interp(lhs, env)
        val rhsV = interp(rhs, env)
        (lhsV, rhsV) match {
            case (VNum(r), VNum(l)) =>
                op match {
                    case OpAdd => VNum(r + l)
                    case OpSub => VNum(r - l)
                    case OpMul => VNum(r * l)
                    case OpDiv => VNum(r / l)
                    case _ => sys.error(s"Cannot $op $lhsV and $rhsV")
                }
            case (VBool(r), VBool(l)) =>
                op match {
                    case OpAnd => VBool(r && l)
                    case OpOr => VBool(r || l)
                    case _ => sys.error(s"Cannot $op $lhsV and $rhsV")
                }
            case _ => sys.error(s"Cannot $op $lhsV and $rhsV")
        }
    }

    private def interp(expr: Expression, env: Env): Value = expr match {

        case Num(n) => VNum(n)

        case Neg(e) =>
            interp(e, env) match {
                case VNum(n) => VNum(-n)
                case _ => sys.error(s"Cannot negate $e")
            }

        case Add(lhs, rhs) => interpBinOp(lhs, rhs, OpAdd, env)
        case Sub(lhs, rhs) => interpBinOp(lhs, rhs, OpSub, env)
        case Mul(lhs, rhs) => interpBinOp(lhs, rhs, OpMul, env)
        case Div(lhs, rhs) => interpBinOp(lhs, rhs, OpDiv, env)


        case Bool(b) => VBool(b)

        case Not(e) =>
            interp(e, env) match {
                case VBool(b) => VBool(!b)
                case _ => sys.error(s"Cannot negate $e")
            }

        case And(lhs, rhs) => interpBinOp(lhs, rhs, OpAnd, env)
        case Or(lhs, rhs) => interpBinOp(lhs, rhs, OpOr, env)
    }

    def interpret(expr: Expression): Value = interp(expr, Map.empty)
}
