package myscript

import myscript.language.Token._

object Lexer {
    def lex(text: String): List[Token] = lexInner(text.toList)

    private val alpha = """[a-zA-Z]""".r
    private val numeric = """[0-9]""".r

    private def lexInner(stream: List[Char]): List[Token] = stream match {
        case Nil => Nil
        case head :: tail => head match {
            case ' ' | '\n' =>
                lexInner(tail)

            case '+' | '-' | '*' | '/' | '&' | '|' | '!' =>
                (TOperator, head.toString) :: lexInner(tail)

            case '(' | ')' =>
                (TPunctuation, head.toString) :: lexInner(tail)

            case alpha() =>
                scanSymbol(tail, head.toString)

            case numeric() =>
                scanNumber(tail, head.toString)

            case _ =>
                sys.error(s"Unknown symbol: $head")
        }
    }

    private def scanNumber(stream: List[Char], acc: String): List[Token] = stream match {
        case Nil => (TNumber, acc) :: Nil
        case head :: tail => head match {
            case numeric() | '.' =>
                scanNumber(tail, acc + head)
            case _ =>
                (TNumber, acc) :: lexInner(stream)
        }
    }

    private def scanSymbol(stream: List[Char], acc: String): List[Token] = stream match {
        case Nil => (TSymbol, acc) :: Nil
        case head :: tail => head match {
            case alpha() =>
                scanSymbol(tail, acc + head)
            case _ =>
                (TSymbol, acc) :: lexInner(stream)
        }
    }
}
