import yapl._
import yapl.language.{BoolV, ErrorV, NumV, Value, VoidV}

import scala.io.Source
import scala.util.Using

object Main {
    def main(args: Array[String]): Unit = {
        val program = Using(Source.fromFile(args.head)) {
            _.mkString
        }

        if (program.isFailure) {
            println(s"Cannot open file ${args.head}")
            return
        }

        run(program.get, args.head) match {
            case NumV(n) => println(s">> $n : Num")
            case BoolV(b) => println(s">> $b : Bool")
            case VoidV() => println(s">> Void")
            case ErrorV() =>
        }
    }

    def run(program: String, file: String = "", debug: Boolean = false): Value = {
        val tokens = try {
            Lexer.lex(program, file)
        } catch {
            case e: LexerException =>
                if (debug) throw e
                println(s"Lexer error in file '${e.file}' at line ${e.line}: ${e.msg}")
                return ErrorV()
        }

        val (expr, globals) = try {
            Parser.parse(tokens)
        } catch {
            case e: ParserException =>
                if (debug) throw e
                println(s"Parser error in file '${e.file}' at line ${e.line}: ${e.msg}")
                return ErrorV()
        }

        try {
            Interpreter.interpret(expr, globals)
        } catch {
            case e: TypeError =>
                if (debug) throw e
                println(s"Type error in file '$file' at line : ${e.msg}")
                ErrorV()
            case e: SemanticError =>
                if (debug) throw e
                println(s"Semantic error in file '$file' at line : ${e.msg}")
                ErrorV()
        }
    }
}
