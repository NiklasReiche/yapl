import myscript._

object Main {
    def main(args: Array[String]): Unit = {
        val tokens = Lexer.lex(args.head)
        println("> Lexer output: " + tokens)

        val expr = Parser.parse(tokens)
        println("> Parser output: " + expr)

        val result = Interpreter.interpret(expr)
        print(">> Interpreter result: " + result + "\n")
    }
}
