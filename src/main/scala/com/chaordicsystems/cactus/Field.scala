package com.chaordicsystems.cactus

import com.chaordicsystems.cactus.Operator._
import com.sksamuel.elastic4s.ElasticDsl._
import com.sksamuel.elastic4s.QueryDefinition

case class Field(value: String) extends Enumeration  {

  private def isFieldNested: Boolean = value.contains(".")

  private def getPath: String = value.split('.')(0)
  private def getName: String = value.split('.')(1)

  private def nestedOrElse(query: QueryDefinition): QueryDefinition =
    if (isFieldNested) nestedQuery(getPath).query { query }
    else query

  private def comparativeQueryHandler(op: Operation, args: Any): QueryDefinition = {
    op match {
      case Operator.LT  => rangeQuery(value) to args includeUpper false
      case Operator.GT  => rangeQuery(value) from args includeLower false
      case Operator.LE  => rangeQuery(value) to args includeUpper true
      case Operator.GE  => rangeQuery(value) from args includeUpper true
      case Operator.NE  => matchQuery(value, args)
      case Operator.EQ  => matchQuery(value, args)
      case _             => throw InvalidOperatorException()
    }
  }

  private def comparativeQuery(op: Operation, args: Any): QueryDefinition = {
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

  def EQ(args: Any): QueryDefinition = nestedOrElse(bool { must { comparativeQuery(Operator.EQ, args)} })
  def EQ(args: Any, typeEnabled: Boolean): QueryDefinition = {
    if (typeEnabled) {
      nestedOrElse(bool{
        must(
          matchQuery(s"$getPath.name", getName),
          Field(s"$getPath.${getType(args)}") comparativeQuery (Operator.EQ, args)
        )
      })
    } else EQ(args)
  }

  def LT(args: Any): QueryDefinition = nestedOrElse(bool { must { comparativeQuery(Operator.LT, args) } })
  def LT(args: Any, typeEnabled: Boolean): QueryDefinition = {
    if (typeEnabled) {
      nestedOrElse(bool{
        must(
          matchQuery(s"$getPath.name", getName),
          Field(s"$getPath.${getType(args)}") comparativeQuery (Operator.LT, args)
        )
      })
    } else LT(args)
  }

  def GT(args: Any): QueryDefinition = nestedOrElse(bool { must { comparativeQuery(Operator.GT, args)} })
  def GT(args: Any, typeEnabled: Boolean): QueryDefinition = {
    if (typeEnabled) {
      nestedOrElse(bool{
        must(
          matchQuery(s"$getPath.name", getName),
          Field(s"$getPath.${getType(args)}") comparativeQuery (Operator.GT, args)
        )
      })
    } else GT(args)
  }

  def LE(args: Any): QueryDefinition = nestedOrElse(bool { must { comparativeQuery(Operator.LE, args)} })
  def LE(args: Any, typeEnabled: Boolean): QueryDefinition = {
    if (typeEnabled) {
      nestedOrElse(bool{
        must(
          matchQuery(s"$getPath.name", getName),
          Field(s"$getPath.${getType(args)}") comparativeQuery (Operator.LE, args)
        )
      })
    } else LE(args)
  }

  def GE(args: Any): QueryDefinition = nestedOrElse(bool { must { comparativeQuery(Operator.GE, args) } })
  def GE(args: Any, typeEnabled: Boolean): QueryDefinition = {
    if (typeEnabled) {
      nestedOrElse(bool{
        must(
          matchQuery(s"$getPath.name", getName),
          Field(s"$getPath.${getType(args)}") comparativeQuery (Operator.GE, args)
        )
      })
    } else GE(args)
  }

  def NE(args: Any): QueryDefinition = nestedOrElse(bool { not { comparativeQuery(Operator.NE, args) } })
  def NE(args: Any, typeEnabled: Boolean): QueryDefinition = {
    if (typeEnabled) {
      nestedOrElse(bool{
        must(matchQuery(s"$getPath.name", getName))
        not(Field(s"$getPath.${getType(args)}") comparativeQuery (Operator.NE, args))
      })
    } else NE(args)
  }

  def ALL(args: Any): QueryDefinition = EQ(args)
  def ALL(args: List[Any]): QueryDefinition =
    if (args.length == 1) ALL(args.head)
    else bool { must { args.map(arg => EQ(arg)) } }

  def ANY(args: List[Any]): QueryDefinition = nestedOrElse(bool { should(args.map(arg => matchQuery(value, arg))) minimumShouldMatch 1 })

  def *(op: Operation, args: Any, typeEnabled: Boolean): QueryDefinition = {
    op match {
      case Operator.NE => NE(args, typeEnabled)
      case Operator.EQ => EQ(args, typeEnabled)
      case Operator.LT => LT(args, typeEnabled)
      case Operator.GT => GT(args, typeEnabled)
      case Operator.LE => LE(args, typeEnabled)
      case Operator.GE => GE(args, typeEnabled)
    }
  }
}
