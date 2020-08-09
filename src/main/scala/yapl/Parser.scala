package yapl

import yapl.language._

class ParserException(val file: String, val line: Integer, val msg: String) extends Exception

object Parser {
    def parse(tokens: List[Token]): (Expression, List[Global]) = parseTopLevelExpressions(tokens)

    private def handleError(expected: String, tokens: List[Token]) = tokens match {
        case token :: _ =>
            throw new ParserException(token.meta.file, token.meta.line, s"Expected $expected but got '${token.symbol}''")
        case Nil =>
            throw new ParserException("", -1, s"Expected $expected but got EOF")
    }

    private def parseTopLevelExpressions(tokens: List[Token]): (Expression, List[Global]) = tokens match {
        case TPunctuation("(", _) :: TSymbol("global", _) :: _ =>
            val (global, r1) = parseGlobal(tokens)
            val (expr, lst) = parseTopLevelExpressions(r1)
            (expr, global :: lst)

        case TPunctuation("(", _) :: _ =>
            val (expr, r1) = parseExpression(tokens)
            val (_, lst) = parseTopLevelExpressions(r1)
            (expr, lst)

        case token :: _ =>
            throw new ParserException(token.meta.file, token.meta.line, s"Expected 'global' or expression, but got $token")

        case Nil => (Void(), Nil)
    }

    private def parseGlobal(tokens: List[Token]): (Global, List[Token]) = tokens match {
        case TPunctuation("(", _) :: TSymbol("global", _) :: tail =>
            val (id, r1) = parseIdentifier(tail)
            val (expr, r2) = parseExpression(r1)
            r2 match {
                case TPunctuation(")", _) :: r3 => (new Global(id, expr), r3)
                case _ => handleError("')'", r2)
            }
    }

    private def parseExpression(tokens: List[Token]): (Expression, List[Token]) = tokens match {
        case TPunctuation("(", _) :: tail =>
            val (expr, r1) = parseOperation(tail)
            r1 match {
                case TPunctuation(")", _) :: tail => (expr, tail)
                case _ => handleError("')'", r1)
            }

        case TNumber(n, _) :: tail => (Num(n.toDouble), tail)

        case TSymbol(s, _) :: tail => s match {
            case "true" => (Bool(true), tail)
            case "false" => (Bool(false), tail)
            case _ => (Id(Symbol(s)), tail)
        }

        case _ => handleError("expression", tokens)
    }

    private def parseOperation(tokens: List[Token]): (Expression, List[Token]) = tokens match {
        case TOperator(_, _) :: _ => parseNAryOperator(tokens)
        case TSymbol(_, _) :: _ => parseKeyword(tokens)
        case _ => handleError("operation", tokens)
    }

    private def parseIdentifier(tokens: List[Token]): (Id, List[Token]) = tokens match {
        case TSymbol(name, _) :: tail => (Id(Symbol(name)), tail)
        case _ => handleError("identifier", tokens)
    }

    private def parseKeyword(tokens: List[Token]): (Expression, List[Token]) = tokens match {
        case TSymbol("let", _) :: tail => tail match {
            case TPunctuation("[", _) :: _ =>
                val (declarations, r1) = parseLetPairs(tail)
                val (body, r2) = parseExpression(r1)
                (Let(declarations, body), r2)
            case _ => handleError("'['", tail)
        }

        case TSymbol("func", _) :: tail => tail match {
            case TPunctuation("[", _) :: tail =>
                val (params, r1) = parseNIdentifiers(tail)
                r1 match {
                    case TPunctuation("]", _) :: tail =>
                        val (body, r2) = parseExpression(tail)
                        (Fun(params, body), r2)

                    case _ => handleError("']'", r1)
                }
            case _ => handleError("'['", tail)
        }

        case TSymbol("call", _) :: tail =>
            val (id, r1) = parseIdentifier(tail)
            val (args, r2) = parseNExpressions(r1)
            (App(id, args), r2)

        case TSymbol("if", _) :: tail =>
            val (test, r1) = parseExpression(tail)
            val (ifCase, r2) = parseExpression(r1)
            val (elseCase, r3) = parseExpression(r2)
            (If(test, ifCase, elseCase), r3)

        case TSymbol("seq", _) :: tail =>
            val (statements, r1) = parseNExpressions(tail)
            (Seq(statements), r1)

        case TSymbol("set", _) :: tail =>
            val (id, r1) = parseIdentifier(tail)
            val (value, r2) = parseExpression(r1)
            (Set(id, value), r2)

        case _ => handleError("keyword", tokens)
    }

    private def parseNAryOperator(tokens: List[Token]): (Expression, List[Token]) = tokens match {
        case head :: tail =>
            val (operands, rest) = parseNExpressions(tail)
            head match {
                case TOperator("+", _) => (Add(operands), rest)
                case TOperator("-", _) => (Sub(operands), rest)
                case TOperator("*", _) => (Mul(operands), rest)
                case TOperator("/", _) => (Div(operands), rest)

                case TOperator("&", _) => (And(operands), rest)
                case TOperator("|", _) => (Or(operands), rest)
                case TOperator("!", _) => (Not(operands), rest)

                case TOperator("=", _) => (Equal(operands), rest)
                case TOperator("<", _) => (Less(operands), rest)
                case TOperator(">", _) => (Greater(operands), rest)

                case TOperator(symbol, meta) => throw new ParserException(meta.file, meta.line, s"Unknown operator $symbol")
            }
        case Nil => throw new ParserException("", -1, s"Expected operator but got EOF")
    }

    private def parseNIdentifiers(tokens: List[Token]): (List[Id], List[Token]) = tokens match {
        case head :: _ => head match {
            case TSymbol(_, _) =>
                val (id, r1) = parseIdentifier(tokens)
                val (listIds, rest) = parseNIdentifiers(r1)
                (id :: listIds, rest)
            case _ => (Nil, tokens)
        }
        case _ => throw new ParserException("", -1, s"Expected identifier but got EOF")
    }

    private def parseNExpressions(tokens: List[Token]): (List[Expression], List[Token]) = tokens match {
        case head :: _ => head match {
            case TPunctuation("(", _) | TNumber(_, _) | TSymbol(_, _) =>
                val (expr, r1) = parseExpression(tokens)
                val (listExpr, rest) = parseNExpressions(r1)
                (expr :: listExpr, rest)
            case _ => (Nil, tokens)
        }
        case _ => throw new ParserException("", -1, s"Expected expression but got EOF")
    }

    private def parseLetPairs(tokens: List[Token]): (List[(Id, Expression)], List[Token]) = tokens match {
        case head :: tail => head match {
            case TPunctuation("[", _) =>
                val (id, r1) = parseIdentifier(tail)
                val (value, r2) = parseExpression(r1)
                r2 match {
                    case TPunctuation("]", _) :: tail =>
                        val (lst, r3) = parseLetPairs(tail)
                        ((id, value) :: lst, r3)
                    case _ => handleError("']'", r2)
                }
            case _ => (Nil, tokens)
        }
        case Nil => throw new ParserException("", -1, s"Expected '[' but gor EOF")
    }
}
