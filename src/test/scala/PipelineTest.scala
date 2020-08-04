import myscript.language.{NumV, Void}

import scala.language.implicitConversions

class PipelineTest extends org.scalatest.FunSuite {

    test("if") {
        assertResult(NumV(1)) {
            Main.run("(if true 1 2)", debug = true)
        }

        assertResult(NumV(2)) {
            Main.run("(if false 1 2)", debug = true)
        }

        assertResult(NumV(2)) {
            Main.run("(if (& true false) 1 2)", debug = true)
        }

        assertResult(NumV(1)) {
            Main.run("(if (| true false) 1 2)", debug = true)
        }
    }

    test("let") {
        assertResult(NumV(6)) {
            Main.run(
                """
                  (let x (+ 5 2)
                        (- x 1)
                  )
                  """, debug = true)
        }
    }

    test("fun") {
        assertResult(NumV(44)) {
            Main.run(
                """
                  (let a (func 42)
                    (let f (func x y (+ x y))
                      (call f 2 (call a))
                    )
                  )
                  """, debug = true)
        }
    }

    test("rec") {
        assertResult(NumV(120)) {
            Main.run(
                """
                  (rec factorial (func n
                                      (if (= n 1)
                                            1
                                            (* n (call factorial (- n 1)))
                                      )
                                 )
                    (call factorial 5)
                  )
                  """, debug = true)
        }
    }

    test("seq") {
        assertResult(Void()) {
            Main.run("(seq)", debug = true)
        }

        assertResult(NumV(42)) {
            Main.run(
                """
                  |(seq
                  |     (+ 5 1)
                  |     5
                  |     42
                  |)
                  |""".stripMargin, debug = true)
        }
    }

    test("set") {
        assertResult(NumV(42)) {
            Main.run(
                """
                  |(let x 0
                  | (seq
                  |     (set x 40)
                  |     (+ x 2)
                  | )
                  |)
                  |""".stripMargin, debug = true)
        }
    }
}
