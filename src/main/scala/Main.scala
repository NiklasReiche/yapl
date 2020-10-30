import yapl._
import yapl.language.{BoolV, ClassV, Closure, ErrorV, NumV, Object, Value, VoidV}

import scala.io.Source
import scala.util.Using

object Main {
    def main(args: Array[String]): Unit = {
        if (args.isEmpty) {
            println("Usage: java -jar yapl.jar <file>")
            return
        }

        val program = Using(Source.fromFile(args.head)) {
            _.mkString
        }

        if (program.isFailure) {
            println(s"Cannot open file ${args.head}")
            return
        }

        run(program.get, args.head, debug = args contains "-d") match {
            case NumV(n) => println(s">> $n : Num")
            case BoolV(b) => println(s">> $b : Bool")
            case Closure(params, _, _) => println(s">> [function] : Closure")
            case ClassV(fields, methods) => println(s">> [class]: Class")
            case Object(c, fieldValues) => println(s">> [object]: Object")
            case VoidV() => println(s">> Void")
            case ErrorV() =>
        }
    }

    def run(program: String, file: String = "", debug: Boolean = false): Value = {
        def handleLexerException(e: LexerException): Unit = {
            if (debug) throw e
            println(s"Lexer error in file '${e.file}' at line ${e.line}: ${e.msg}")
        }
        def handleParserException(e: ParserException): Unit = {
            if (debug) throw e
            println(s"Parser error in file '${e.file}' at line ${e.line}: ${e.msg}")
        }

        val tokens = try {
            Lexer.lex(program, file)
        } catch {
            case e: LexerException => handleLexerException(e); return ErrorV()
        }

        val (expr, globals, imports) = try {
            Parser.parse(tokens)
        } catch {
            case e: ParserException => handleParserException(e); return ErrorV()
        }

        val stdlib = try {
            Preprocessor.importStdlib()
        } catch {
            case e: LexerException => handleLexerException(e); return ErrorV()
            case e: ParserException => handleParserException(e); return ErrorV()
        }

        val importedGlobals = try {
            if (!file.isEmpty)
                Preprocessor.importModules(file.substring(0, file.lastIndexOf("/") + 1), imports)
            else
                Nil
        } catch {
            case e: LexerException => handleLexerException(e); return ErrorV()
            case e: ParserException => handleParserException(e); return ErrorV()
        }

        try {
            Interpreter.interpret(expr, stdlib concat globals concat importedGlobals)
        } catch {
            case e: TypeError =>
                if (debug) throw e
                println(s"Type error in file '${e.file}' at line ${e.line}: ${e.msg}")
                ErrorV()
            case e: SemanticError =>
                if (debug) throw e
                println(s"Semantic error in file '${e.file}' at line ${e.line}: ${e.msg}")
                ErrorV()
        }
    }
}
