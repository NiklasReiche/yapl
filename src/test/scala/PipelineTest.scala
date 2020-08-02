import myscript.language.NumV

import scala.language.implicitConversions

class PipelineTest extends org.scalatest.FunSuite {

    test("if") {
        assertResult(NumV(1)) {
            Main.run("(if true 1 2)")
        }

        assertResult(NumV(2)) {
            Main.run("(if false 1 2)")
        }

        assertResult(NumV(2)) {
            Main.run("(if (& true false) 1 2)")
        }

        assertResult(NumV(1)) {
            Main.run("(if (| true false) 1 2)")
        }
    }

    test("let") {
        assertResult(NumV(6)) {
            Main.run("""
                  (let x (+ 5 2)
                        (- x 1)
                  )
                  """)
        }
    }

    test("fun") {
        assertResult(NumV(44)) {
            Main.run("""
                  (let a (fun 42)
                    (let f (fun x y (+ x y))
                      (app f 2 (app a))
                    )
                  )
                  """)
        }
    }

    test("rec") {
        assertResult(NumV(120)) {
            Main.run("""
                  (rec factorial (fun n
                                      (if (= n 1)
                                            1
                                            (* n (app factorial (- n 1)))
                                      )
                                 )
                    (app factorial 5)
                  )
                  """)
        }
    }
}
