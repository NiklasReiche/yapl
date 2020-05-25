package myscript.language

object Token {
    sealed trait TokenType

    type Token = (TokenType, String)

    case object TNumber extends TokenType

    case object TOperator extends TokenType

    case object TPunctuation extends TokenType

    case object TSymbol extends TokenType
}
