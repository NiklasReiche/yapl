package myscript

import myscript.language.Token._
import myscript.language._

object Parser {
    def parse(tokens: List[Token]): (Expression, List[Global]) = parseTopLevelExpressions(tokens)

    private def parseTopLevelExpressions(tokens: List[Token]): (Expression, List[Global]) = tokens match {
        case head :: tail => head match {
            case (TPunctuation, "(") => tail match {
                case head :: tail2 => head match {
                    case (TSymbol, "global") =>
                        val (global, r1) = parseGlobal(tail)
                        r1 match {
                            case (TPunctuation, ")") :: r2 =>
                                val (expr, lst) = parseTopLevelExpressions(r2)
                                (expr, global :: lst)
                            case _ => sys.error(s"Expected ')', but got $r1")
                        }

                    case _ =>
                        val (expr, r1) = parseExpression(tokens)
                        val (_, lst) = parseTopLevelExpressions(r1)
                        (expr, lst)
                }
            }
            case _ => sys.error(s"Expected global or expression, but got $head")
        }
        case Nil => (Void(), Nil)
    }

    private def parseGlobal(tokens: List[Token]): (Global, List[Token]) = tokens match {
        case head :: tail => head match {
            case (TSymbol, "global") =>
                val (id, r1) = parseIdentifier(tail)
                val (expr, r2) = parseExpression(r1)
                (new Global(id, expr), r2)
        }
    }

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

            case _ => sys.error(s"Expected <(> or <Number> or <Identifier>, but got $head")
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
                val (declarations, r1) = parseLetPairs(tail)
                val (body, r2) = parseExpression(r1)
                (Let(declarations, body), r2)

            case (TSymbol, "func") => tail match {
                case (TPunctuation, "[") :: tail =>
                    val (params, r) = parseNIdentifiers(tail)
                    r match {
                        case (TPunctuation, "]") :: tail =>
                            val (body, r2) = parseExpression(tail)
                            (Fun(params, body), r2)
                        case _ => sys.error(s"expected ']', but got $r")
                    }
                case _ => sys.error(s"expected '[', but got ${tail.head}")
            }

            case (TSymbol, "call") =>
                val (id, r1) = parseIdentifier(tail)
                val (args, r2) = parseNExpressions(r1)
                (App(id, args), r2)

            case (TSymbol, "if") =>
                val (test, r1) = parseExpression(tail)
                val (ifCase, r2) = parseExpression(r1)
                val (elseCase, r3) = parseExpression(r2)
                (If(test, ifCase, elseCase), r3)

            case (TSymbol, "seq") =>
                val (statements, r1) = parseNExpressions(tail)
                (Seq(statements), r1)

            case (TSymbol, "set") =>
                val (id, r1) = parseIdentifier(tail)
                val (value, r2) = parseExpression(r1)
                (Set(id, value), r2)

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

    private def parseLetPairs(tokens: List[Token]): (List[(Id, Expression)], List[Token]) = tokens match {
        case head :: tail => head match {
            case (TPunctuation, "[") =>
                val (id, r1) = parseIdentifier(tail)
                val (value, r2) = parseExpression(r1)
                r2 match {
                    case head :: tail => head match {
                        case (TPunctuation, "]") =>
                            val (lst, r3) = parseLetPairs(tail)
                            ((id, value) :: lst, r3)
                        case _ => sys.error(s"expected ']', but got $head")
                    }
                }
            case _ => (Nil, tokens)
        }
        case Nil => sys.error("EOF")
    }
}
