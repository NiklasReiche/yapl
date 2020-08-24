package yapl

import yapl.language._

class ParserException(val file: String, val line: Integer, val msg: String) extends Exception

object Parser {
    def parse(tokens: List[Token]): (Expression, List[Global], List[Import]) = parseTopLevelExpressions(tokens)

    private def handleError(expected: String, tokens: List[Token]) = tokens match {
        case token :: _ =>
            throw new ParserException(token.meta.file, token.meta.line, s"Expected $expected but got '${token.symbol}''")
        case Nil =>
            throw new ParserException("", -1, s"Expected $expected but got EOF")
    }

    private def parseTopLevelExpressions(tokens: List[Token]): (Expression, List[Global], List[Import]) = tokens match {
        case TPunctuation("(", _) :: TSymbol("import", _) :: tail => tail match {
            case TSymbol(module, _) :: tail => tail match {
                case TPunctuation(")", _) :: tail =>
                    val (expr, lst, lst2) = parseTopLevelExpressions(tail)
                    (expr, lst, Import(module) :: lst2)
                case _ => handleError("')'", tail)
            }
            case _ => handleError("module name", tail)
        }

        case TPunctuation("(", _) :: TSymbol("global", _) :: _ =>
            val (global, r1) = parseGlobal(tokens)
            val (expr, lst, lst2) = parseTopLevelExpressions(r1)
            (expr, global :: lst, lst2)

        case TPunctuation("(", _) :: _ =>
            val (expr, r1) = parseExpression(tokens)
            val (_, lst, lst2) = parseTopLevelExpressions(r1)
            (expr, lst, lst2)

        case token :: _ =>
            throw new ParserException(token.meta.file, token.meta.line, s"Expected 'global' or expression or import, but got $token")

        case Nil => (Void(MetaInfo("", -1)), Nil, Nil)
    }

    private def parseGlobal(tokens: List[Token]): (Global, List[Token]) = tokens match {
        case TPunctuation("(", _) :: TSymbol("global", _) :: tail =>
            val (id, r1) = parseIdentifier(tail)
            val (expr, r2) = parseExpression(r1)
            r2 match {
                case TPunctuation(")", _) :: r3 => (Global(id, expr), r3)
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

        case TNumber(n, metaInfo) :: tail => (Num(n.toDouble, metaInfo), tail)

        case TSymbol(s, metaInfo) :: tail => s match {
            case "true" => (Bool(b = true, metaInfo), tail)
            case "false" => (Bool(b = false, metaInfo), tail)
            case _ => (Id(Symbol(s), metaInfo), tail)
        }

        case _ => handleError("expression", tokens)
    }

    private def parseOperation(tokens: List[Token]): (Expression, List[Token]) = tokens match {
        case TOperator(_, _) :: _ => parseNAryOperator(tokens)
        case TSymbol(_, _) :: _ => parseKeyword(tokens)
        case _ => handleError("operation", tokens)
    }

    private def parseIdentifier(tokens: List[Token]): (Id, List[Token]) = tokens match {
        case TSymbol(name, metaInfo) :: tail => (Id(Symbol(name), metaInfo), tail)
        case _ => handleError("identifier", tokens)
    }

    private def parseKeyword(tokens: List[Token]): (Expression, List[Token]) = tokens match {
        case TSymbol("let", metaInfo) :: tail => tail match {
            case TPunctuation("[", _) :: _ =>
                val (declarations, r1) = parseLetPairs(tail)
                val (body, r2) = parseExpression(r1)
                (Let(declarations, body, metaInfo), r2)
            case _ => handleError("'['", tail)
        }

        case TSymbol("func", metaInfo) :: tail => tail match {
            case TPunctuation("[", _) :: tail =>
                val (params, r1) = parseNIdentifiers(tail)
                r1 match {
                    case TPunctuation("]", _) :: tail =>
                        val (body, r2) = parseExpression(tail)
                        (Func(params, body, metaInfo), r2)

                    case _ => handleError("']'", r1)
                }
            case _ => handleError("'['", tail)
        }

        case TSymbol("call", metaInfo) :: tail =>
            val (id, r1) = parseIdentifier(tail)
            r1 match {
                case TPunctuation("[", _) :: tail =>
                    val (args, r2) = parseNExpressions(tail)
                    r2 match {
                        case TPunctuation("]", _) :: tail =>
                            (FunctionCall(id, args, metaInfo), tail)
                        case _ => handleError("']'", r1)
                    }
                case _ => handleError("'['", tail)
            }

        case TSymbol("if", metaInfo) :: tail =>
            val (test, r1) = parseExpression(tail)
            val (ifCase, r2) = parseExpression(r1)
            val (elseCase, r3) = parseExpression(r2)
            (If(test, ifCase, elseCase, metaInfo), r3)

        case TSymbol("seq", metaInfo) :: tail =>
            val (statements, r1) = parseNExpressions(tail)
            (Seq(statements, metaInfo), r1)

        case TSymbol("set", metaInfo) :: tail =>
            val (id, r1) = parseIdentifier(tail)
            val (value, r2) = parseExpression(r1)
            (Set(id, value, metaInfo), r2)

        case TSymbol("class", metaInfo) :: tail =>
            val (fields, methods, r1) = parseClassBody(tail)
            (Class(fields, methods, metaInfo), r1)

        case TSymbol("create", metaInfo) :: tail =>
            val (classExpr, r1) = parseExpression(tail)
            r1 match {
                case TPunctuation("[", _) :: tail =>
                    val (fieldValues, r2) = parseNExpressions(tail)
                    r2 match {
                        case TPunctuation("]", _) :: tail =>
                            (Create(classExpr, fieldValues, metaInfo), tail)
                        case _ => handleError("]", r2)
                    }
                case _ => handleError("[", r1)
            }

        case TSymbol("field-get", metaInfo) :: tail =>
            val (obj, r1) = parseExpression(tail)
            val (id, r2) = parseIdentifier(r1)
            (FieldGet(obj, id, metaInfo), r2)

        case TSymbol("field-set", metaInfo) :: tail =>
            val (obj, r1) = parseExpression(tail)
            val (id, r2) = parseIdentifier(r1)
            val (value, r3) = parseExpression(r2)
            (FieldSet(obj, id, value, metaInfo), r3)

        case TSymbol("method-call", metaInfo) :: tail =>
            val (obj, r1) = parseExpression(tail)
            val (id, r2) = parseIdentifier(r1)
            r2 match {
                case TPunctuation("[", _) :: tail =>
                    val (args, r3) = parseNExpressions(tail)
                    r3 match {
                        case TPunctuation("]", _) :: tail =>
                            (MethodCall(obj, id, args, metaInfo), tail)
                        case _ => handleError("']'", r1)
                    }
                case _ => handleError("'['", tail)
            }

        case _ => handleError("keyword", tokens)
    }

    private def parseNAryOperator(tokens: List[Token]): (Expression, List[Token]) = {
        def checkOperands(operands: List[Expression], amount: Int, op: String, meta: MetaInfo): Unit = {
            if (operands.length < amount)
                throw new ParserException(meta.file, meta.line, s"Expected $amount or more arguments for operator '$op' but got ${operands.length}")
        }

        tokens match {
            case head :: tail =>
                val (operands, rest) = parseNExpressions(tail)
                head match {
                    case TOperator("+", meta) =>
                        checkOperands(operands, 1, "+", meta)
                        (Add(operands, meta), rest)

                    case TOperator("-", meta) =>
                        checkOperands(operands, 1, "-", meta)
                        (Sub(operands, meta), rest)

                    case TOperator("*", meta) =>
                        checkOperands(operands, 2, "+", meta)
                        (Mul(operands, meta), rest)

                    case TOperator("/", meta) =>
                        checkOperands(operands, 2, "/", meta)
                        (Div(operands, meta), rest)

                    case TOperator("&", meta) =>
                        checkOperands(operands, 2, "&", meta)
                        (And(operands, meta), rest)

                    case TOperator("|", meta) =>
                        checkOperands(operands, 2, "|", meta)
                        (Or(operands, meta), rest)

                    case TOperator("!", meta@MetaInfo(file, line)) =>
                        if (operands.length != 1)
                            throw new ParserException(file, line, s"Expected 1 argument for operator '!' but got ${operands.length}")
                        (Not(operands, meta), rest)

                    case TOperator("=", meta@MetaInfo(file, line)) =>
                        if (operands.length != 2)
                            throw new ParserException(file, line, s"Expected 2 arguments for operator '=' but got ${operands.length}")
                        (Equal(operands, meta), rest)

                    case TOperator("<", meta@MetaInfo(file, line)) =>
                        if (operands.length != 2)
                            throw new ParserException(file, line, s"Expected 2 arguments for operator '<' but got ${operands.length}")
                        (Less(operands, meta), rest)

                    case TOperator(">", meta@MetaInfo(file, line)) =>
                        if (operands.length != 2)
                            throw new ParserException(file, line, s"Expected 2 arguments for operator '>' but got ${operands.length}")
                        (Greater(operands, meta), rest)

                    case TOperator(symbol, meta) => throw new ParserException(meta.file, meta.line, s"Unknown operator $symbol")
                }
            case Nil => throw new ParserException("", -1, s"Expected operator but got EOF")
        }
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

    private def parseClassBody(tokens: List[Token]): (List[Id], List[(Id, Expression)], List[Token]) = tokens match {
        case TPunctuation("(", _) :: TSymbol("field", _) :: tail =>
            val (id, r1) = parseIdentifier(tail)
            r1 match {
                case TPunctuation(")", _) :: tail =>
                    val (fields, methods, r2) = parseClassBody(tail)
                    (id :: fields, methods, r2)
                case _ => handleError("')'", r1)
            }

        case TPunctuation("(", _) :: TSymbol("method", m) :: tail =>
            val (id, r1) = parseIdentifier(tail)
            r1 match {
                case TPunctuation("[", _) :: tail =>
                    val (params, r2) = parseNIdentifiers(tail)
                    r2 match {
                        case TPunctuation("]", _) :: tail =>
                            val (body, r3) = parseExpression(tail)
                            r3 match {
                                case TPunctuation(")", _) :: tail =>
                                    val (fields, methods, r3) = parseClassBody(tail)
                                    (fields, (id, Func(params, body, m)) :: methods, r3)
                            }
                        case _ => handleError("']'", r2)
                    }
                case _ => handleError("'['", r1)
            }

        case TPunctuation(")", _) :: _ => (Nil, Nil, tokens)
    }
}
