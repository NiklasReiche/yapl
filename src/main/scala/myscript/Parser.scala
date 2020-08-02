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

            case (TSymbol, s) => s match {
                case "true" => (Bool(true), tail)
                case "false" => (Bool(false), tail)
                case _ => (Id(Symbol(s)), tail)
            }

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
            case (TSymbol, "let") | (TSymbol, "rec") =>
                val (id, r1) = parseIdentifier(tail)
                val (value, r2) = parseExpression(r1)
                val (body, r3) = parseExpression(r2)
                head match {
                    case (TSymbol, "let") => (Let(id, value, body), r3)
                    case (TSymbol, "rec") => (Rec(id, value, body), r3)
                }

            case (TSymbol, "fun") =>
                val (params, r) = parseNIdentifiers(tail)
                val (body, r2) = parseExpression(r)
                (Fun(params, body), r2)

            case (TSymbol, "app") =>
                val (id, r1) = parseIdentifier(tail)
                val (args, r2) = parseNExpressions(r1)
                (App(id, args), r2)

            case (TSymbol, "if") =>
                val (test, r1) = parseExpression(tail)
                val (ifCase, r2) = parseExpression(r1)
                val (elseCase, r3) = parseExpression(r2)
                (If(test, ifCase, elseCase), r3)

            case _ => sys.error(s"Expected keyword, but got $head")
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

                    case (TOperator, "=") => (Equal(operands), rest)
                    case (TOperator, "<") => (Less(operands), rest)
                    case (TOperator, ">") => (Greater(operands), rest)
                    case _ => sys.error(s"Encountered unknown operator <$head>")
                }
            case Nil => sys.error("Expected operator, but got EOF")
        }

    private def parseNIdentifiers(tokens: List[Token]): (List[Id], List[Token]) = tokens match {
        case head :: _ => head match {
            case (TSymbol, _) =>
                val (id, r1) = parseIdentifier(tokens)
                val (listIds, rest) = parseNIdentifiers(r1)
                (id :: listIds, rest)
            case _ => (Nil, tokens)
        }
        case _ => sys.error("EOF")
    }

    private def parseNExpressions(tokens: List[Token]): (List[Expression], List[Token]) = tokens match {
        case head :: _ => head match {
            case (TPunctuation, "(") | (TNumber, _) | (TSymbol, _) =>
                val (expr, r1) = parseExpression(tokens)
                val (listExpr, rest) = parseNExpressions(r1)
                (expr :: listExpr, rest)
            case _ => (Nil, tokens)
        }
        case _ => sys.error("EOF")
    }
}
