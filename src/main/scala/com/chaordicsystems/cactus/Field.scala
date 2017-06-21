package com.chaordicsystems.cactus

import com.chaordicsystems.cactus.Operator.Operator
import com.sksamuel.elastic4s.ElasticDsl.{bool, matchQuery, must, nestedQuery, not, rangeQuery, should, termQuery}
import com.sksamuel.elastic4s.QueryDefinition

case class Field(value: String) extends Operation {
  private val isFieldNested: Boolean = value.contains(".")

  private val getPath: String = value.split('.')(0)
  private val getName: String = value.split('.')(1)

  private def nestedOrElse(query: QueryDefinition): QueryDefinition =
    if (isFieldNested) nestedQuery(getPath).query { query }
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
    println(args.getClass)
    args match {
      case _: Boolean => termQuery(value, args)
      case d: Double  => comparativeQueryHandler(op, d)
      case b: BigInt  => comparativeQueryHandler(op, b.toInt)
      case s: String  => comparativeQueryHandler(op, s)
      case _          => throw InvalidCactusQueryFormatException()
    }
  }

  private def getType(args: Any): String = {
    args match {
      case _: BigInt  => "value_int"
      case _: Boolean => "value_bool"
      case _: Double  => "value_float"
      case _: String  => "value_str"
      case _          => throw InvalidCactusQueryFormatException()
    }
  }

  def EQ(args: Any, typeEnabled: Boolean = false): QueryDefinition = {
    if (typeEnabled) {
      nestedOrElse(bool{
        must(
          matchQuery(s"$getPath.name", getName),
          Field(s"$getPath.${getType(args)}") comparativeQuery (Operator.EQ, args)
        )
      })
    } else nestedOrElse(bool { must { comparativeQuery(Operator.EQ, args)} })
  }

  def LT(args: Any, typeEnabled: Boolean): QueryDefinition = {
    if (typeEnabled) {
      nestedOrElse(bool{
        must(
          matchQuery(s"$getPath.name", getName),
          Field(s"$getPath.${getType(args)}") comparativeQuery (Operator.LT, args)
        )
      })
    } else nestedOrElse(bool { must { comparativeQuery(Operator.LT, args) } })
  }

  def GT(args: Any, typeEnabled: Boolean): QueryDefinition = {
    if (typeEnabled) {
      nestedOrElse(bool{
        must(
          matchQuery(s"$getPath.name", getName),
          Field(s"$getPath.${getType(args)}") comparativeQuery (Operator.GT, args)
        )
      })
    } else nestedOrElse(bool { must { comparativeQuery(Operator.GT, args)} })
  }

  def LE(args: Any, typeEnabled: Boolean): QueryDefinition = {
    if (typeEnabled) {
      nestedOrElse(bool{
        must(
          matchQuery(s"$getPath.name", getName),
          Field(s"$getPath.${getType(args)}") comparativeQuery (Operator.LE, args)
        )
      })
    } else nestedOrElse(bool { must { comparativeQuery(Operator.LE, args)} })
  }

  def GE(args: Any, typeEnabled: Boolean): QueryDefinition = {
    if (typeEnabled) {
      nestedOrElse(bool{
        must(
          matchQuery(s"$getPath.name", getName),
          Field(s"$getPath.${getType(args)}") comparativeQuery (Operator.GE, args)
        )
      })
    } else nestedOrElse(bool { must { comparativeQuery(Operator.GE, args) } })
  }

  def NE(args: Any, typeEnabled: Boolean): QueryDefinition = {
    if (typeEnabled) {
      nestedOrElse(bool{
        must(matchQuery(s"$getPath.name", getName))
        not(Field(s"$getPath.${getType(args)}") comparativeQuery (Operator.NE, args))
      })
    } else nestedOrElse(bool { not { comparativeQuery(Operator.NE, args) } })
  }

  def ALL(args: List[Any]): QueryDefinition =
    if (args.length == 1) EQ(args.head)
    else bool { must { args.map(arg => EQ(arg)) } }

  def ANY(args: List[Any]): QueryDefinition = nestedOrElse(bool { should(args.map(arg => matchQuery(value, arg))) minimumShouldMatch 1 })
}
