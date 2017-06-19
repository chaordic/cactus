package com.chaordicsystems.cactus

import com.chaordicsystems.cactus.Operation._
import com.sksamuel.elastic4s.ElasticDsl._
import com.sksamuel.elastic4s._
import org.json4s._

case class InvalidCactusQueryFormatException(m: String = "There's a problem with your query, please check the documentation.") extends Exception
case class Cactus(op: String, field: Option[String], args: Any)

object Parser {
  def mapToCactus(values: Map[String, Any]): Cactus = {
    val op = values("op").toString
    val field = if (values.contains("field")) Option(values("field").toString) else None
    val args = values("args")
    Cactus(op, field, args)
  }

  def comparativeQueryHandler(op: Operation, property: String, value: Any): QueryDefinition = {
    op match {
      case LT => rangeQuery(property) to value includeUpper false
      case GT => rangeQuery(property) from value includeLower false
      case LE => rangeQuery(property) to value includeUpper true
      case GE => rangeQuery(property) from value includeUpper true
      case _  => matchQuery(property, value)
    }
  }

  def comparativeQuery(op: Operation, property: String, value: Any): QueryDefinition = {
    value match {
      case _: Boolean => termQuery(property, value)
      case _: Double  => comparativeQueryHandler(op, property, value.asInstanceOf[Double])
      case _: BigInt  => comparativeQueryHandler(op, property, value.asInstanceOf[BigInt].toInt)
      case _: String  => comparativeQueryHandler(op, property, value.asInstanceOf[String])
      case _          => throw InvalidCactusQueryFormatException()
    }
  }

  def handleDetails(cactus: Cactus): NestedQueryDefinition = {
    val name = cactus.field.get.split("\\.")(1)
    val valueType = cactus.args match {
      case _: BigInt  => "value_int"
      case _: Boolean => "value_bool"
      case _: Double  => "value_float"
      case _: String  => "value_str"
      case _          => throw InvalidCactusQueryFormatException()
    }

    Operation.withName(cactus.op) match {
      case NE => nestedQuery("details").query {
        bool {
          must(matchQuery("details.name", name))
          not(comparativeQuery(NE, s"details.$valueType", cactus.args))
        }
      }
      case op => nestedQuery("details").query {
        bool {
          must (
            matchQuery("details.name", name),
            comparativeQuery(op, s"details.$valueType", cactus.args)
          )
        }
      }
    }
  }

  def handleCatOrTag(cactus: Cactus): NestedQueryDefinition = {
    val field = if (cactus.field.isDefined) cactus.field.get else throw InvalidCactusQueryFormatException()
    val listArgs = cactus.args.asInstanceOf[List[Any]]

    Operation.withName(cactus.op) match {
      case ALL => nestedQuery(field).query {
        bool {
          must(listArgs.map(x => matchQuery(s"$field.id", x)))
        }
      }
      case ANY => nestedQuery(field).query {
        bool {
          bool {
            should(listArgs.map(x => matchQuery(s"$field.id", x)))
          }
        }
      }
    }
  }

  def validateAndTranslate(cactus: Cactus): QueryDefinition = {
    if (Operation.isLogical(cactus.op) && cactus.args.isInstanceOf[List[Any]]) {
      val listArgs = cactus.args.asInstanceOf[List[Any]]

      if (listArgs.length < 2) throw InvalidCactusQueryFormatException()

      Operation.withName(cactus.op) match {
        case AND => bool {
          must(listArgs.map(x => validateAndTranslate(mapToCactus(x.asInstanceOf[Map[String, Any]]))))
        }
        case OR => bool {
          should(listArgs.map(x => validateAndTranslate(mapToCactus(x.asInstanceOf[Map[String, Any]]))))
        }
      }
    }
    else if (Operation.isComparative(cactus.op) || Operation.isContains(cactus.op)) {
      cactus.field match {
        case x if x.get.startsWith("details.")             => handleDetails(cactus)
        case x if x.get == "tags" || x.get == "categories" => handleCatOrTag(cactus)
        case _                                             => throw InvalidCactusQueryFormatException()
      }
    }
    else {
      throw InvalidCactusQueryFormatException()
    }
  }

  def cactusToES(jsonValue: JValue): QueryDefinition = {
    implicit val formats = DefaultFormats

    validateAndTranslate(jsonValue.extract[Cactus])
  }
}
