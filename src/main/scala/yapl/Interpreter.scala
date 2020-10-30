package yapl

import yapl.language._

class TypeError(val file: String, val line: Int, val msg: String) extends Exception

class SemanticError(val file: String, val line: Int, val msg: String) extends Exception

object Interpreter {
    def interpret(expr: Expression, globals: List[Global]): Value = {
        val emptyStore = BasicStore[Value](Map.empty)
        val (globalEnv, store) = interpGlobals(globals, emptyStore.malloc(VoidV())._2)
        val (loc, s) = interp(expr, globalEnv, store)
        s.lookup(loc)
    }

    type Location = Int
    type Env = scala.collection.Map[Symbol, Location]

    var _currentLocation = 0

    def nextLocation: Location = {
        _currentLocation += 1
        _currentLocation
    }

    private def interpGlobals(globals: List[Global], startStore: Store[Value]): (Env, Store[Value]) =
        globals.foldLeft((Map.empty: Env, startStore))((acc, global) => {
            var (env, store) = (collection.mutable.Map() ++ acc._1, acc._2)
            val (loc, s1) = interp(global.value, env, store)
            env += global.id.name -> loc
            (env, s1)
        })

    private def interp(expr: Expression, env: Env, store: Store[Value]): (Location, Store[Value]) =
        expr match {
            // Terminal expressions ------------------------------------------------------------------------------------
            case Num(n, _) => store.malloc(NumV(n))
            case Bool(b, _) => store.malloc(BoolV(b))
            case Func(params, body, _) => store.malloc(Closure(params, body, env))
            case Id(name, _) => (env(name), store)

            // Numeric operators ---------------------------------------------------------------------------------------
            case Add(operands, _) =>
                val (rawValues, s1) = interpNAryNumOperands(operands, env, store)
                s1.malloc(NumV(calcNAryOp[Double](rawValues, _ + _, 0)))

            case Mul(operands, _) =>
                val (rawValues, s1) = interpNAryNumOperands(operands, env, store)
                s1.malloc(NumV(calcNAryOp[Double](rawValues, _ * _, 1)))

            case Sub(operands, _) =>
                val (rawValues, s1) = interpNAryNumOperands(operands, env, store)
                if (operands.length == 1)
                    s1.malloc(NumV(calcNAryOp[Double](rawValues, _ - _, 0)))
                else
                    s1.malloc(NumV(calcNAryOp[Double](rawValues.tail, _ - _, rawValues.head)))

            case Div(operands, _) =>
                val (rawValues, s1) = interpNAryNumOperands(operands, env, store)
                s1.malloc(NumV(calcNAryOp[Double](rawValues.tail, _ / _, rawValues.head)))


            // Boolean operators ---------------------------------------------------------------------------------------
            case And(operands, _) =>
                val (rawValues, s1) = interpNAryBoolOperands(operands, env, store)
                s1.malloc(BoolV(calcNAryOp[Boolean](rawValues, _ && _, neutralElement = true)))

            case Or(operands, _) =>
                val (rawValues, s1) = interpNAryBoolOperands(operands, env, store)
                s1.malloc(BoolV(calcNAryOp[Boolean](rawValues, _ || _, neutralElement = false)))

            case Not(operands, meta) =>
                val (loc, s1) = interp(operands.head, env, store)
                s1.lookup(loc) match {
                    case BoolV(b) => s1.malloc(BoolV(!b))
                    case _ => throw new TypeError(meta.file, meta.line, s"Cannot apply operator '!' to ${operands.head}")
                }

            // Comparison operators ------------------------------------------------------------------------------------
            case Equal(operands, meta) =>
                val (loc1, s1) = interp(operands.head, env, store)
                val (loc2, s2) = interp(operands.tail.head, env, s1)
                val (op1, op2) = (s1.lookup(loc1), s2.lookup(loc2))
                (op1, op2) match {
                    case (NumV(a), NumV(b)) => s2.malloc(BoolV(a == b))
                    case (BoolV(a), BoolV(b)) => s2.malloc(BoolV(a == b))
                    case (c1@Closure(_, _, _), c2@Closure(_, _, _)) => s2.malloc(BoolV(c1 equals c2))
                    case (c1@ClassV(_, _), c2@ClassV(_, _)) => s2.malloc(BoolV(c1 equals c2))
                    case (o1@Object(_, _), o2@Object(_, _)) => s2.malloc(BoolV(o1 equals o2))
                    case (v1, v2) => throw new TypeError(meta.file, meta.line, s"Type mismatch for operator '=', $v1 and $v2")
                }

            case Less(operands, meta) =>
                val (loc1, s1) = interp(operands.head, env, store)
                val (loc2, s2) = interp(operands.tail.head, env, s1)
                val (op1, op2) = (s1.lookup(loc1), s2.lookup(loc2))
                (op1, op2) match {
                    case (NumV(a), NumV(b)) => s2.malloc(BoolV(a < b))
                    case (v1, v2) => throw new TypeError(meta.file, meta.line, s"Type mismatch for operator '<', $v1 and $v2")
                }

            case Greater(operands, meta) =>
                val (loc1, s1) = interp(operands.head, env, store)
                val (loc2, s2) = interp(operands.tail.head, env, s1)
                val (op1, op2) = (s1.lookup(loc1), s2.lookup(loc2))
                (op1, op2) match {
                    case (NumV(a), NumV(b)) => s2.malloc(BoolV(a > b))
                    case (v1, v2) => throw new TypeError(meta.file, meta.line, s"Type mismatch for operator '>', $v1 and $v2")
                }

            // Special constructs --------------------------------------------------------------------------------------
            case Let(declarations, body, _) =>
                val (letEnv, letStore) = interpIdValuePairsRec(declarations, env, env, store)
                interp(body, letEnv, letStore)

            case FunctionCall(funExpr, args, meta) =>
                val (closureLoc, s1) = interp(funExpr, env, store)
                s1.lookup(closureLoc) match {
                    case c@Closure(_, body, _) =>
                        val (funcEnv, funcStore) = interpClosureCall(c, args, env, s1, meta)
                        interp(body, funcEnv, funcStore)
                    case v =>
                        throw new TypeError(funExpr.meta.file, funExpr.meta.line, s"Expected closure but got $v")
                }

            case Set(Id(name, _), valueExpr, _) =>
                val (loc, s1) = interp(valueExpr, env, store)
                s1.update(env(name), s1.lookup(loc))

            // Control structures --------------------------------------------------------------------------------------
            case If(test, ifCase, elseCase, _) =>
                val (loc, s1) = interp(test, env, store)
                s1.lookup(loc) match {
                    case BoolV(b) =>
                        if (b)
                            interp(ifCase, env, s1)
                        else
                            interp(elseCase, env, s1)

                    case v =>
                        throw new TypeError(test.meta.file, test.meta.line, s"Expected boolean value but got $v")
                }

            case Cond(branches, metaInfo) =>
                var s = store
                branches.find(b => {
                    val (testLoc, s1) = interp(b._1, env, s)
                    s = s1
                    s.lookup(testLoc) match {
                        case BoolV(b) => b
                        case v => throw new TypeError(b._1.meta.file, b._1.meta.line, s"Expected boolean value but got $v")
                    }
                }) match {
                    case Some((_, bodyExpr)) => interp(bodyExpr, env, s)
                    case _ => (0, s)
                }

            case Seq(statements, _) =>
                statements.foldLeft((0: Location, store: Store[Value]))(
                    (acc, expr) => interp(expr, env, acc._2)
                )

            // Classes -------------------------------------------------------------------------------------------------
            case Class(fields, methods, classMeta) =>
                if (fields.distinct.length != fields.length)
                    throw new SemanticError(classMeta.file, classMeta.line, s"Class has duplicate fields")

                val (methodIdPairs, classStore) = methods.foldLeft(Map.empty: Map[Id, Closure], store)((acc, method) => {
                    val (methodIdPairs, accStore) = acc
                    val (id, expr) = method

                    if (methodIdPairs.contains(id))
                        throw new SemanticError(id.meta.file, id.meta.line, s"Class already has a field with name ${id.name}")

                    val (closureLoc, s1) = interp(expr, env, accStore)
                    s1.lookup(closureLoc) match {
                        case c@Closure(_, _, _) => (methodIdPairs + (id -> c), s1)
                        case v => throw new TypeError(expr.meta.file, expr.meta.line, s"Expected function but got $v")
                    }
                })
                classStore.malloc(ClassV(fields, methodIdPairs))

            case Create(classExpr, fieldValuesExpr, createMeta) =>
                val (classValLoc, s0) = interp(classExpr, env, store)
                s0.lookup(classValLoc) match {
                    case c@ClassV(fields, _) =>
                        if (fields.length != fieldValuesExpr.length)
                            throw new SemanticError(createMeta.file, createMeta.line, s"Expected ${fields.length} arguments, but got ${fieldValuesExpr.length}")

                        val (fieldValues, s1) = fieldValuesExpr.foldRight(Nil: List[Location], store)((expr, acc) => {
                            val (lst, s) = acc
                            val (loc, s1) = interp(expr, env, s)
                            // call-by-value
                            val (newLoc, s2) = s1.malloc(s1.lookup(loc))
                            (newLoc :: lst, s2)
                        })
                        s1.malloc(Object(c, fieldValues))
                    case _ => throw new TypeError(createMeta.file, createMeta.line, s"Can only create objects from classes")
                }

            case FieldGet(objExpr, field, meta) =>
                val (objLoc, s1) = interp(objExpr, env, store)
                s1.lookup(objLoc) match {
                    case Object(ClassV(fields, _), fieldValues) =>
                        (fields zip fieldValues).find(p => p._1.name == field.name) match {
                            case Some(value) => (value._2, s1)
                            case None => throw new SemanticError(meta.file, meta.line, s"Object has no field '${field.name}'")
                        }
                    case v => throw new TypeError(objExpr.meta.file, objExpr.meta.line, s"Expected object but got $v")
                }

            case FieldSet(objExpr, field, valueExpr, meta) =>
                val (objLoc, s1) = interp(objExpr, env, store)
                val (valueLoc, s2) = interp(valueExpr, env, s1)
                s1.lookup(objLoc) match {
                    case Object(ClassV(fields, _), fieldValues) =>
                        (fields zip fieldValues).find(p => p._1.name == field.name) match {
                            case Some(v) => s2.update(v._2, s2.lookup(valueLoc))
                            case None => throw new SemanticError(meta.file, meta.line, s"Object has no field '${field.name}'")
                        }
                    case v => throw new TypeError(objExpr.meta.file, objExpr.meta.line, s"Expected object but got $v")
                }

            case MethodCall(objExpr, methodId, args, meta) =>
                val (objLoc, s1) = interp(objExpr, env, store)
                s1.lookup(objLoc) match {
                    case Object(ClassV(fields, methods), fieldValueLocations) => methods.get(methodId) match {
                        case c@Some(Closure(_, body, _)) =>
                            val (funcEnv, funcStore) = interpClosureCall(c.value, args, env, s1, meta)
                            val extendedFuncEnv = funcEnv concat
                                (fields map (i => i.name) zip fieldValueLocations).toMap concat
                                Map(Symbol("this") -> objLoc)
                            interp(body, extendedFuncEnv, funcStore)
                        case None =>
                            throw new SemanticError(methodId.meta.file, methodId.meta.line, s"Object has no method with name ${methodId.name}")
                    }
                    case v => throw new TypeError(objExpr.meta.file, objExpr.meta.line, s"Expected object but got $v")
                }
        }

    private def calcNAryOp[A](operands: List[A], op: (A, A) => A, neutralElement: A): A =
        operands.foldLeft(neutralElement)(op)

    private def interpNAryNumOperands(operands: List[Expression], env: Env, store: Store[Value]) =
        operands.foldRight((Nil: List[Double], store: Store[Value]))((expr, acc) => {
            val (lst, s) = acc
            val (loc, s1) = interp(expr, env, s)
            s1.lookup(loc) match {
                case NumV(n) => (n :: lst, s1)
                case value => throw new TypeError(expr.meta.file, expr.meta.line, s"Expected <Num> but got $value")
            }
        })

    private def interpNAryBoolOperands(operands: List[Expression], env: Env, store: Store[Value]) =
        operands.foldRight((Nil: List[Boolean], store: Store[Value]))((expr, acc) => {
            val (lst, s) = acc
            val (loc, s1) = interp(expr, env, s)
            s1.lookup(loc) match {
                case BoolV(b) => (b :: lst, s1)
                case value => throw new TypeError(expr.meta.file, expr.meta.line, s"Expected <Bool> but got $value")
            }
        })

    private def interpClosureCall(closure: Closure, args: List[Expression], env: Env, store: Store[Value], meta: MetaInfo): (Env, Store[Value]) = {
        if (closure.params.length != args.length)
            throw new SemanticError(meta.file, meta.line, s"Expected ${closure.params.length} arguments, but got ${args.length}")

        interpIdValuePairs(closure.params zip args, env, closure.env, store)
    }

    /**
     * Creates bindings for multiple identifiers.
     *
     * @param pairs   The pairs of (identifier = value).
     * @param evalEnv The environment in which the vales are evaluated.
     * @param accEnv  The environment to which the new bindings are appended.
     * @param store   The store to use during evaluation and appending.
     * @return A new environment and store containing bindings for each identifier in pairs.
     */
    private def interpIdValuePairs(pairs: List[(Id, Expression)], evalEnv: Env, accEnv: Env, store: Store[Value]): (Env, Store[Value]) =
        pairs.foldLeft((accEnv, store))((acc, pair) => {
            val (e, s) = acc
            val (id, valueExpr) = pair
            val (loc, s1) = interp(valueExpr, evalEnv, s)
            val (newLoc, s2) = s1.malloc(s1.lookup(loc))
            (e ++ Map(id.name -> newLoc), s2)
        })

    /**
     * Creates recursive bindings for multiple identifiers.
     *
     * @param pairs   The pairs of (identifier = value).
     * @param evalEnv The environment in which the vales are evaluated.
     * @param accEnv  The environment to which the new bindings are appended.
     * @param store   The store to use during evaluation and appending.
     * @return A new environment and store containing bindings for each identifier in pairs.
     */
    private def interpIdValuePairsRec(pairs: List[(Id, Expression)], evalEnv: Env, accEnv: Env, store: Store[Value]): (Env, Store[Value]) =
        pairs.foldLeft((accEnv, store))((acc, pair) => {
            val recEnv = collection.mutable.Map() ++ evalEnv
            val (id, valueExpr) = pair
            val (loc, s1) = interp(valueExpr, recEnv, acc._2)
            val (newLoc, s2) = s1.malloc(s1.lookup(loc))
            recEnv += id.name -> newLoc
            (acc._1 ++ Map(id.name -> newLoc), s2)
        })
}
