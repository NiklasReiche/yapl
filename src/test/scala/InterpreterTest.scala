import myscript.language._
import myscript.Interpreter._

import scala.language.implicitConversions

class InterpreterTest extends org.scalatest.FunSuite {
    implicit def doubleToNum(n: Double): Num = Num(n)
    implicit def floatToNum(n: Float): Num = Num(n)
    implicit def intToNum(n: Int): Num = Num(n)
    implicit def booleanToBool(b: Boolean): Bool = Bool(b)

    test("add") {
        assertResult(NumV(12)) {
            interpret(Add(List(2, 3, 7)))
        }
    }

    test("sub") {
        assertResult(NumV(-1)) {
            interpret(Sub(List(2, 3)))
        }
    }

    test("mul") {
        assertResult(NumV(6)) {
            interpret(Mul(List(2, 3)))
        }
    }

    test("div") {
        assertResult(NumV(2.5)) {
            interpret(Div(List(5, 2)))
        }
    }

    test("arith") {
        assertResult(NumV(7)) {
            interpret(
                Add(List(
                    Sub(List(5, 2)),
                    Mul(List(
                        2,
                        Div(List(10, 5))
                    ))
                ))
            )
        }
    }
}