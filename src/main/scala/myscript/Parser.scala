package myscript

import myscript.language.Token._
import myscript.language._

object Parser {
    def parse(tokens: List[Token]): Expression = parseExpression(tokens)._1

    private def parseExpression(tokens: List[Token]): (Expression, List[Token]) = tokens match {
        case head :: tail => head match {
            case (TPunctuation, "(") =>
                val (expr, r1) = parseOperation(tail)
                r1 match {
                    case (TPunctuation, ")") :: rest => (expr, rest)
                    case _ => sys.error(s"Expected ')', but got $r1")
                }

            case (TNumber, n) =>
                (Num(n.toDouble), tail)

            case (TSymbol, s) =>
                (Id(Symbol(s)), tail)

            case _ => sys.error("Expected <(> or <Number> or <Identifier>")
        }
        case Nil => sys.error("Expected expression, but got EOF")
    }

    private def parseOperation(tokens: List[Token]): (Expression, List[Token]) = tokens match {
        case head :: _ => head match {
            case (TOperator, _) =>
                parseNAryOperator(tokens)
            case (TSymbol, _) =>
                parseKeyword(tokens)
            case _ => sys.error(s"Expected operation, but got $head")
        }
        case _ => sys.error("Expected operation, but got EOF")
    }

    private def parseIdentifier(tokens: List[Token]): (Id, List[Token]) = tokens match {
        case head :: tail => head match {
            case (TSymbol, name) => (Id(Symbol(name)), tail)
            case _ => sys.error(s"Expected an identifier, but got $head")
        }
        case Nil => sys.error("Expected an identifier, but got EOF")
    }

    private def parseKeyword(tokens: List[Token]): (Expression, List[Token]) = tokens match {
        case head :: tail => head match {
            case (TSymbol, "let") =>
                val (id, r1) = parseIdentifier(tail)
                val (value, r2) = parseExpression(r1)
                val (body, rest) = parseExpression(r2)
                (Let(id, value, body), rest)
            case _ => sys.error(s"Expected <let>, but got $head")
        }
        case _ => sys.error("Expected keyword, but got EOF")
    }

    private def parseNAryOperator(tokens: List[Token]): (Expression, List[Token]) = {
        def parseNextExpression(tokensInner: List[Token]): (List[Expression], List[Token]) = tokensInner match {
            case head :: _ => head match {
                case (TPunctuation, ")") => (Nil, tokensInner)
                case _ =>
                    val (expr, r) = parseExpression(tokensInner)
                    val (lExpr, rest) = parseNextExpression(r)
                    (expr :: lExpr, rest)
            }
            case Nil => sys.error("Expected <)> or expression, but got EOF")
        }

        tokens match {
            case head :: tail =>
                val (operands, rest) = parseNextExpression(tail)
                head match {
                    case (TOperator, "+") => (Add(operands), rest)
                    case (TOperator, "-") => (Sub(operands), rest)
                    case (TOperator, "*") => (Mul(operands), rest)
                    case (TOperator, "/") => (Div(operands), rest)
                    case (TOperator, "&") => (And(operands), rest)
                    case (TOperator, "|") => (Or(operands), rest)
                    case (TOperator, "!") => (Not(operands), rest)
                    case _ => sys.error(s"Encountered unknown operator <$head>")
                }
            case Nil => sys.error("Expected operator, but got EOF")
        }
    }
}
