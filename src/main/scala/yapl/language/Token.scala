package yapl.language

case class MetaInfo(file: String, line: Integer)

sealed abstract class Token(val meta: MetaInfo) {
    def symbol: String
}

case class TOperator(symbol: String, tokenInfo: MetaInfo) extends Token(tokenInfo)
case class TPunctuation(symbol: String, tokenInfo: MetaInfo) extends Token(tokenInfo)
case class TNumber(symbol: String, tokenInfo: MetaInfo) extends Token(tokenInfo)
case class TSymbol(symbol: String, tokenInfo: MetaInfo) extends Token(tokenInfo)
