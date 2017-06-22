package com.chaordicsystems.cactus

import com.chaordicsystems.cactus.Operator.Operator
import com.chaordicsystems.cactus.Validator._
import com.sksamuel.elastic4s.ElasticDsl.{bool, matchQuery, must, nestedQuery, not, rangeQuery, should, termQuery}
import com.sksamuel.elastic4s.QueryDefinition

case class Field(value: String) {
  private val isFieldNested: Boolean = value.contains(".")

  private val path: String = value.split('.')(0)
  private val name: String = value.split('.')(1)

  private def nestedOrElse(query: QueryDefinition): QueryDefinition =
    if (isFieldNested) nestedQuery(path).query { query }
    else query

  private def comparativeQueryHandler(op: Operator, args: Any): QueryDefinition = {
    op match {
      case Operator.LT  => rangeQuery(value) to args includeUpper false
      case Operator.GT  => rangeQuery(value) from args includeLower false
      case Operator.LE  => rangeQuery(value) to args includeUpper true
      case Operator.GE  => rangeQuery(value) from args includeUpper true
      case Operator.NE  => matchQuery(value, args)
      case Operator.EQ  => matchQuery(value, args)
      case _            => throw InvalidOperatorException()
    }
  }

  private def comparativeQuery(op: Operator, args: Any): QueryDefinition = {
    args match {
      case _: Boolean => termQuery(value, args)
      case d: Double  => comparativeQueryHandler(op, d)
      case b: BigInt  => comparativeQueryHandler(op, b.toInt)
      case s: String  => comparativeQueryHandler(op, s)
      case _          => throw InvalidValueType()
    }
  }

  private def getType(args: Any): String = {
    args match {
      case _: BigInt  => "value_int"
      case _: Boolean => "value_bool"
      case _: Double  => "value_float"
      case _: String  => "value_str"
      case _          => throw InvalidValueType()
    }
  }

  private def resolveOperator(op: Operator, args: Any, typeEnabled: Boolean): QueryDefinition = {
    if (typeEnabled) {
      nestedOrElse(bool{
        must(
          matchQuery(s"$path.name", name),
          Field(s"$path.${getType(args)}") comparativeQuery (op, args)
        )
      })
    } else nestedOrElse(bool { must { comparativeQuery(op, args) } })
  }

  def EQ(args: Any, typeEnabled: Boolean = false): QueryDefinition = resolveOperator(Operator.EQ, args, typeEnabled)

  def LT(args: Any, typeEnabled: Boolean = false): QueryDefinition = resolveOperator(Operator.LT, args, typeEnabled)

  def GT(args: Any, typeEnabled: Boolean = false): QueryDefinition = resolveOperator(Operator.GT, args, typeEnabled)

  def LE(args: Any, typeEnabled: Boolean = false): QueryDefinition = resolveOperator(Operator.LE, args, typeEnabled)

  def GE(args: Any, typeEnabled: Boolean = false): QueryDefinition = resolveOperator(Operator.GE, args, typeEnabled)

  def NE(args: Any, typeEnabled: Boolean = false): QueryDefinition = {
    if (typeEnabled) {
      nestedOrElse(bool{
        must(matchQuery(s"$path.name", name))
        not(Field(s"$path.${getType(args)}") comparativeQuery (Operator.NE, args))
      })
    } else nestedOrElse(bool { not { comparativeQuery(Operator.NE, args) } })
  }

  def ALL(args: List[Any]): QueryDefinition =
    if (args.length == 1) EQ(args.head)
    else bool { must { args.map(EQ(_)) } }

  def ANY(args: List[Any]): QueryDefinition = nestedOrElse(bool { should(args.map(arg => matchQuery(value, arg))) minimumShouldMatch 1 })
}
