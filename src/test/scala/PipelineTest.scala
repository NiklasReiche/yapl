import yapl.language.{BoolV, NumV, VoidV}

class PipelineTest extends org.scalatest.FunSuite {

    test("arithmetic expressions") {
        assertResult(NumV(12)) {
            Main.run("(+ 2 3 7)")
        }
        assertResult(NumV(-2)) {
            Main.run("(- 2 3 1)")
        }
        assertResult(NumV(12)) {
            Main.run("(* 2 3 2)")
        }
        assertResult(NumV(2.5)) {
            Main.run("(/ 5 2)")
        }
    }

    test("boolean expressions") {
        assertResult(BoolV(true)) {
            Main.run("(| true false true false)")
        }
        assertResult(BoolV(false)) {
            Main.run("(& true false true)")
        }
        assertResult(BoolV(true)) {
            Main.run("(! false)")
        }
    }

    test("comparison expressions") {
        assertResult(BoolV(false)) {
            Main.run("(= 2 3)")
        }
        assertResult(BoolV(true)) {
            Main.run("(= 2 2)")
        }
        assertResult(BoolV(true)) {
            Main.run("(< 2 3)")
        }
        assertResult(BoolV(false)) {
            Main.run("(< 2 2)")
        }
        assertResult(BoolV(false)) {
            Main.run("(> 2 3)")
        }
        assertResult(BoolV(true)) {
            Main.run("(> 3 2)")
        }
    }

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

    test("simple let") {
        assertResult(NumV(6)) {
            Main.run(
                """
                  |(let [x (+ 5 2)]
                  |  (- x 1)
                  |)
                  |""".stripMargin, debug = true)
        }
    }

    test("multiple let") {
        assertResult(NumV(52)) {
            Main.run(
                """
                  |(let [x (+ 1 3)]
                  |     [y 42]
                  |     [z (* 3 2)]
                  |  (+ x y z)
                  |)
                  |""".stripMargin, debug = true)
        }
    }

    test("let func") {
        assertResult(NumV(44)) {
            Main.run(
                """
                  |(let [a (func [] 42)]
                  |     [f (func [x y] (+ x y))]
                  |  (call f 2 (call a))
                  |)
                  |""".stripMargin, debug = true)
        }
    }

    test("let rec") {
        assertResult(NumV(120)) {
            Main.run(
                """
                  |(let [factorial (func [n]
                  |                  (if (= n 1)
                  |                      1
                  |                      (* n (call factorial (- n 1)))
                  |                  )
                  |                )]
                  |  (call factorial 5)
                  |)
                  |""".stripMargin, debug = true)
        }
    }

    test("seq") {
        assertResult(VoidV()) {
            Main.run("(seq)")
        }

        assertResult(NumV(42)) {
            Main.run(
                """
                  |(seq
                  |  (+ 5 1)
                  |  5
                  |  42
                  |)
                  |""".stripMargin, debug = true)
        }
    }

    test("set") {
        assertResult(NumV(42)) {
            Main.run(
                """
                  |(let [x 0]
                  |  (seq
                  |    (set x 40)
                  |    (+ x 2)
                  |  )
                  |)
                  |""".stripMargin, debug = true)
        }
    }

    test("global") {
        assertResult(NumV(29)) {
            Main.run(
                """
                  |(global f (func [x] (* x x)))
                  |(global a 25)
                  |
                  |(let [b 2]
                  |  (+ a (call f b))
                  |)
                  |""".stripMargin, debug = true)
        }

        assertResult(NumV(29)) {
            Main.run(
                """
                  |(global a 25)
                  |
                  |(let [b 2]
                  |  (+ a (call f b))
                  |)
                  |
                  |(global f (func [x] (* x x)))
                  |""".stripMargin, debug = true)
        }

        assertResult(NumV(29)) {
            Main.run(
                """
                  |(global a 25)
                  |
                  |(let [b 2]
                  |  (+ a (call f b))
                  |)
                  |
                  |(+ 1 2)
                  |
                  |(global f (func [x] (* x x)))
                  |""".stripMargin, debug = true)
        }

        assertResult(NumV(120)) {
            Main.run(
                """(global factorial
                  |  (func [n]
                  |    (if (= n 1)
                  |        1
                  |        (* n (call factorial (- n 1)))
                  |    )
                  |  )
                  |)
                  |
                  |(call factorial 5)
                  |""".stripMargin, debug = true)
        }
    }
}
