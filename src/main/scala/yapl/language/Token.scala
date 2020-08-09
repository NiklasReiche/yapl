package yapl.language

case class TokenMeta(file: String, line: Integer)

abstract class Token(val meta: TokenMeta) {
    def symbol: String
}

case class TOperator(symbol: String, tokenInfo: TokenMeta) extends Token(tokenInfo)
case class TPunctuation(symbol: String, tokenInfo: TokenMeta) extends Token(tokenInfo)
case class TNumber(symbol: String, tokenInfo: TokenMeta) extends Token(tokenInfo)
case class TSymbol(symbol: String, tokenInfo: TokenMeta) extends Token(tokenInfo)
