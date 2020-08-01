package myscript

import myscript.language.Token._
import myscript.language._

object Parser {
    def parse(tokens: List[Token]): Expression = parseExpression(tokens)._1

    private def parseExpression(tokens: List[Token]): (Expression, List[Token]) = tokens match {
        case head :: tail => head match {
            case (TPunctuation, "(") =>
                val (expr, rest) = parseOperation(tail)
                rest match {
                    case (TPunctuation, ")") :: rest => (expr, rest)
                    case _ => sys.error(s"Expected ')', but got $rest")
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
                val (id, rest1) = parseIdentifier(tail)
                val (value, rest2) = parseExpression(rest1)
                val (body, rest) = parseExpression(rest2)
                (Let(id, value, body), rest)

            case (TSymbol, "fun") =>
                val (params, rest) = parseNIdentifiers(tail)
                val (body, rest2) = parseExpression(rest)
                (Fun(params, body), rest2)

            case (TSymbol, "app") =>
                val (id, rest1) = parseIdentifier(tail)
                val (args, rest2) = parseNExpressions(rest1)
                (App(id, args), rest2)

            case _ => sys.error(s"Expected <let>, but got $head")
        }
        case _ => sys.error("Expected keyword, but got EOF")
    }

    private def parseNAryOperator(tokens: List[Token]): (Expression, List[Token]) = tokens match {
            case head :: tail =>
                val (operands, rest) = parseNExpressions(tail)
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

    private def parseNIdentifiers(tokens: List[Token]): (List[Id], List[Token]) = tokens match {
        case head :: _ => head match {
            case (TSymbol, _) =>
                val (id, rest) = parseIdentifier(tokens)
                val (listIds, rest2) = parseNIdentifiers(rest)
                (id :: listIds, rest2)
            case _ => (Nil, tokens)
        }
        case _ => sys.error("EOF")
    }

    private def parseNExpressions(tokens: List[Token]): (List[Expression], List[Token]) = tokens match {
        case head :: _ => head match {
            case (TPunctuation, "(") | (TNumber, _) | (TSymbol, _) =>
                val (expr, rest) = parseExpression(tokens)
                val (listExpr, rest2) = parseNExpressions(rest)
                (expr :: listExpr, rest2)
            case _ => (Nil, tokens)
        }
        case _ => sys.error("EOF")
    }
}
