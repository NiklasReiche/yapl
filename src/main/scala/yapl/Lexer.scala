package yapl

import yapl.language.{MetaInfo, TNumber, TOperator, TPunctuation, TSymbol, Token}

import scala.annotation.tailrec

class LexerException(val file: String, val line: Integer, val msg: String) extends Exception

object Lexer {
    def lex(text: String, file: String): List[Token] = {
        this.file = file
        val result = lexInner(text.toList, 1)
        this.file = ""
        result
    }

    private val alpha = """[a-zA-Z]""".r
    private val numeric = """[0-9]""".r
    private var file = ""

    private def lexInner(stream: List[Char], line: Int): List[Token] = stream match {
        case Nil => Nil
        case head :: tail => head match {
            case ' ' | '\t' | '\r' =>
                lexInner(tail, line)

            case '\n' =>
                lexInner(tail, line + 1)

            case '#' =>
                scanComment(tail, line)

            case '+' | '-' | '*' | '/' | '&' | '|' | '!' | '=' | '<' | '>' =>
                TOperator(head.toString, MetaInfo(file, line)) :: lexInner(tail, line)

            case '(' | ')' | '[' | ']' =>
                TPunctuation(head.toString, MetaInfo(file, line)) :: lexInner(tail, line)

            case alpha() =>
                scanSymbol(tail, head.toString, line)

            case numeric() =>
                scanNumber(tail, head.toString, line)

            case _ =>
                throw new LexerException(file, line, s"Unknown symbol: ${escape(head.toString)}")
        }
    }

    @tailrec
    private def scanComment(stream: List[Char], line:Int): List[Token] = stream match {
        case Nil => Nil
        case '\n' :: tail => lexInner(tail, line + 1)
        case _ :: tail => scanComment(tail, line)
    }

    @tailrec
    private def scanNumber(stream: List[Char], acc: String, line: Int): List[Token] = stream match {
        case Nil => TNumber(acc, MetaInfo(file, line)) :: Nil
        case head :: tail => head match {
            case numeric() | '.' =>
                scanNumber(tail, acc + head, line)
            case _ =>
                TNumber(acc, MetaInfo(file, line)) :: lexInner(stream, line)
        }
    }

    @tailrec
    private def scanSymbol(stream: List[Char], acc: String, line: Int): List[Token] = stream match {
        case Nil => TSymbol(acc, MetaInfo(file, line)) :: Nil
        case head :: tail => head match {
            case alpha() | '_' | '-' | '?' | '!' =>
                scanSymbol(tail, acc + head, line)
            case _ =>
                TSymbol(acc, MetaInfo(file, line)) :: lexInner(stream, line)
        }
    }

    private def escape(raw: String): String = {
        import scala.reflect.runtime.universe._
        Literal(Constant(raw)).toString
    }
}
