import myscript.language._
import myscript.interpreter._

import scala.language.implicitConversions

class InterpreterTest extends org.scalatest.FunSuite {
    implicit def doubleToNum(n: Double): Num = Num(n)
    implicit def floatToNum(n: Float): Num = Num(n)
    implicit def intToNum(n: Int): Num = Num(n)
    implicit def booleanToBool(b: Boolean): Bool = Bool(b)

    test("add") {
        assertResult(VNum(5)) {
            interpret(Add(2, 3))
        }
    }

    test("sub") {
        assertResult(VNum(-1)) {
            interpret(Sub(2, 3))
        }
    }

    test("mul") {
        assertResult(VNum(6)) {
            interpret(Mul(2, 3))
        }
    }

    test("div") {
        assertResult(VNum(2.5)) {
            interpret(Div(5, 2))
        }
    }

    test("arith") {
        assertResult(VNum(7)) {
            interpret(
                Add(
                    Sub(5, 2),
                    Mul(
                        2,
                        Div(10, 5)
                    )
                )
            )
        }
    }
}