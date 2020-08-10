import yapl.language.{BoolV, NumV, VoidV}

class PipelineTest extends org.scalatest.FunSpec {

    describe("arithmetic expressions") {
        it ("add") {
            assertResult(NumV(12)) {
                Main.run("(+ 2 3 7)")
            }
        }
        it ("subtract") {
            assertResult(NumV(-2)) {
                Main.run("(- 2 3 1)")
            }
        }
        it ("multiply") {
            assertResult(NumV(12)) {
                Main.run("(* 2 3 2)")
            }
        }
        it("divide") {
            assertResult(NumV(2.5)) {
                Main.run("(/ 5 2)")
            }
        }
    }

    describe("boolean expressions") {
        it("and") {
            assertResult(BoolV(true)) {
                Main.run("(| true false true false)")
            }
        }
        it("or") {
            assertResult(BoolV(false)) {
                Main.run("(& true false true)")
            }
        }
        it ("not") {
            assertResult(BoolV(true)) {
                Main.run("(! false)")
            }
        }
    }

    describe("comparison expressions") {
        it ("equality") {
            assertResult(BoolV(false)) {
                Main.run("(= 2 3)")
            }
            assertResult(BoolV(true)) {
                Main.run("(= 2 2)")
            }
        }
        it ("less than") {
            assertResult(BoolV(true)) {
                Main.run("(< 2 3)")
            }
            assertResult(BoolV(false)) {
                Main.run("(< 2 2)")
            }
        }
        it ("greater than") {
            assertResult(BoolV(false)) {
                Main.run("(> 2 3)")
            }
            assertResult(BoolV(true)) {
                Main.run("(> 3 2)")
            }
        }
    }

    describe("if") {
        it ("") {
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
    }

    describe("let") {
        it("simple") {
            assertResult(NumV(6)) {
                Main.run(
                    """
                      |(let [x (+ 5 2)]
                      |  (- x 1)
                      |)
                      |""".stripMargin, debug = true)
            }
        }

        it("multiple bindings") {
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

        it("function") {
            assertResult(NumV(44)) {
                Main.run(
                    """
                      |(let [a (func [] 42)]
                      |     [f (func [x y] (+ x y))]
                      |  (call f [2 (call a [])])
                      |)
                      |""".stripMargin, debug = true)
            }
        }

        it("recursion") {
            assertResult(NumV(120)) {
                Main.run(
                    """
                      |(let [factorial (func [n]
                      |                  (if (= n 1)
                      |                      1
                      |                      (* n (call factorial [(- n 1)]))
                      |                  )
                      |                )]
                      |  (call factorial [5])
                      |)
                      |""".stripMargin, debug = true)
            }
        }
    }

    describe("seq") {
        it ("empty") {
            assertResult(VoidV()) {
                Main.run("(seq)")
            }

        }

        it("") {
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
    }

    describe("set") {
        it("") {
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
    }

    describe("global") {
        it("") {
            assertResult(NumV(29)) {
                Main.run(
                    """
                      |(global f (func [x] (* x x)))
                      |(global a 25)
                      |
                      |(let [b 2]
                      |  (+ a (call f [b]))
                      |)
                      |""".stripMargin, debug = true)
            }

            assertResult(NumV(29)) {
                Main.run(
                    """
                      |(global a 25)
                      |
                      |(let [b 2]
                      |  (+ a (call f [b]))
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
                      |  (+ a (call f [b]))
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
                      |        (* n (call factorial [(- n 1)]))
                      |    )
                      |  )
                      |)
                      |
                      |(call factorial [5])
                      |""".stripMargin, debug = true)
            }
        }
    }
}
