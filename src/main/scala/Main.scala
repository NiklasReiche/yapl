import myscript._
import myscript.language.Value

object Main {
    def main(args: Array[String]): Unit = {
        run(args.head, debug = false)
    }

    def run(program: String, debug: Boolean): Value = {
        val tokens = Lexer.lex(program)
        if (debug)
            println("> Lexer output: " + tokens)

        val expr = Parser.parse(tokens)
        if (debug)
            println("> Parser output: " + expr)

        val result = Interpreter.interpret(expr)
        if (debug)
            print(">> Interpreter result: " + result + "\n")

        result
    }
}
