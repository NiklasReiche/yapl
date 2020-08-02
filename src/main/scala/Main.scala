import myscript._
import myscript.language.Value

object Main {
    def main(args: Array[String]): Unit = {
        run(args.head)
    }

    def run(program: String): Value = {
        val tokens = Lexer.lex(program)
        println("> Lexer output: " + tokens)

        val expr = Parser.parse(tokens)
        println("> Parser output: " + expr)

        val result = Interpreter.interpret(expr)
        print(">> Interpreter result: " + result + "\n")

        result
    }
}
