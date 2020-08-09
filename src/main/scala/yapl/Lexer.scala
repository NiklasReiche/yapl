package yapl

import yapl.language.{TNumber, TOperator, TPunctuation, TSymbol, Token, TokenMeta}

class LexerException(val file: String, val line: Integer, val msg: String) extends Exception

object Lexer {
    def lex(text: String, file: String): List[Token] = {
        this.file = file
        val result = lexInner(text.toList, 0)
        this.file = ""
        result
    }

    private val alpha = """[a-zA-Z]""".r
    private val numeric = """[0-9]""".r
    private var file = ""

    private def lexInner(stream: List[Char], line: Integer): List[Token] = stream match {
        case Nil => Nil
        case head :: tail => head match {
            case ' ' | '\t' | '\r' =>
                lexInner(tail, line)

            case '\n' =>
                lexInner(tail, line + 1)

            case '+' | '-' | '*' | '/' | '&' | '|' | '!' | '=' | '<' | '>' =>
                TOperator(head.toString, TokenMeta(file, line)) :: lexInner(tail, line)

            case '(' | ')' | '[' | ']' =>
                TPunctuation(head.toString, TokenMeta(file, line)) :: lexInner(tail, line)

            case alpha() =>
                scanSymbol(tail, head.toString, line)

            case numeric() =>
                scanNumber(tail, head.toString, line)

            case _ =>
                throw new LexerException(file, line, s"Unknown symbol: ${escape(head.toString)}")
        }
    }

    private def scanNumber(stream: List[Char], acc: String, line: Integer): List[Token] = stream match {
        case Nil => TNumber(acc, TokenMeta(file, line)) :: Nil
        case head :: tail => head match {
            case numeric() | '.' =>
                scanNumber(tail, acc + head, line)
            case _ =>
                TNumber(acc, TokenMeta(file, line)) :: lexInner(stream, line)
        }
    }

    private def scanSymbol(stream: List[Char], acc: String, line: Integer): List[Token] = stream match {
        case Nil => TSymbol(acc, TokenMeta(file, line)) :: Nil
        case head :: tail => head match {
            case alpha() =>
                scanSymbol(tail, acc + head, line)
            case _ =>
                TSymbol(acc, TokenMeta(file, line)) :: lexInner(stream, line)
        }
    }

    private def escape(raw: String): String = {
        import scala.reflect.runtime.universe._
        Literal(Constant(raw)).toString
    }
}
