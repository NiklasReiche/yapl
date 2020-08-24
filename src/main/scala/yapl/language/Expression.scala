package yapl.language

case class Import(file: String)
case class Global(id: Id, value: Expression)

sealed abstract class Expression(val meta: MetaInfo)

case class Void(metaInfo: MetaInfo) extends Expression(metaInfo)

case class Num(n: Double, metaInfo: MetaInfo) extends Expression(metaInfo)
case class Bool(b: Boolean, metaInfo: MetaInfo) extends Expression(metaInfo)

case class Id(name: Symbol, metaInfo: MetaInfo) extends Expression(metaInfo) {
    override def equals(obj: Any): Boolean = obj match {
        case Id(otherName, _) => otherName.equals(name)
        case _ => false
    }
}

case class Add(operands: List[Expression], metaInfo: MetaInfo) extends Expression(metaInfo)
case class Sub(operands: List[Expression], metaInfo: MetaInfo) extends Expression(metaInfo)
case class Mul(operands: List[Expression], metaInfo: MetaInfo) extends Expression(metaInfo)
case class Div(operands: List[Expression], metaInfo: MetaInfo) extends Expression(metaInfo)

case class Not(operands: List[Expression], metaInfo: MetaInfo) extends Expression(metaInfo)
case class And(operands: List[Expression], metaInfo: MetaInfo) extends Expression(metaInfo)
case class Or(operands: List[Expression], metaInfo: MetaInfo) extends Expression(metaInfo)

case class Equal(operands: List[Expression], metaInfo: MetaInfo) extends Expression(metaInfo)
case class Greater(operands: List[Expression], metaInfo: MetaInfo) extends Expression(metaInfo)
case class Less(operands: List[Expression], metaInfo: MetaInfo) extends Expression(metaInfo)

case class Let(declarations: List[(Id, Expression)], body: Expression, metaInfo: MetaInfo) extends Expression(metaInfo)

case class Set(name: Id, value: Expression, metaInfo: MetaInfo) extends Expression(metaInfo)

case class Func(params: List[Id], body: Expression, metaInfo: MetaInfo) extends Expression(metaInfo)
case class FunctionCall(fun: Expression, args: List[Expression], metaInfo: MetaInfo) extends Expression(metaInfo)

case class If(test: Expression, ifCase: Expression, elseCase: Expression, metaInfo: MetaInfo) extends Expression(metaInfo)
case class Seq(statements: List[Expression], metaInfo: MetaInfo) extends Expression(metaInfo)

case class Class(fields: List[Id], methods: List[(Id, Expression)], metaInfo: MetaInfo) extends Expression(metaInfo)
case class Create(classExpr: Expression, fieldValues: List[Expression], metaInfo: MetaInfo) extends Expression(metaInfo)
case class FieldGet(obj: Expression, field: Id, metaInfo: MetaInfo) extends Expression(metaInfo)
case class FieldSet(obj: Expression, field: Id, value: Expression, metaInfo: MetaInfo) extends Expression(metaInfo)
case class MethodCall(obj: Expression, method: Id, args: List[Expression], metaInfo: MetaInfo) extends Expression(metaInfo)
