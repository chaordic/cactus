package com.chaordicsystems.cactus

import com.chaordicsystems.cactus.Operation._
import com.sksamuel.elastic4s.ElasticDsl._
import com.sksamuel.elastic4s._
import org.json4s._

case class InvalidFormatException(m: String) extends Exception

case class Cactus(op: String, field: Option[String], args: Any)

object Parser {
  def mapToCactus(values: Map[String, Any]): Cactus = {
    val op = values("op").toString
    val field = if (values.contains("field")) Option(values("field").toString) else None
    val args = values("args")
    Cactus(op, field, args)
  }

  def comparativeQuery(op: Operation, property: String, value: String): QueryDefinition = {
    op match {
      case LT => rangeQuery(property) to value
      case GT => rangeQuery(property) from value
      case LE => rangeQuery(property) lte value
      case GE => rangeQuery(property) gte value
      case _  => termQuery(property, value)
    }
  }

  def comparativeQuery(op: Operation, property: String, value: Double): QueryDefinition = {
    op match {
      case LT => rangeQuery(property) to value
      case GT => rangeQuery(property) from value
      case LE => rangeQuery(property) lte value
      case GE => rangeQuery(property) gte value
      case _  => termQuery(property, value)
    }
  }

  def comparativeQuery(op: Operation, property: String, value: Any): QueryDefinition = {
    value match {
      case _: Boolean => termQuery(property, value)
      case _: Double => comparativeQuery(op, property, value.asInstanceOf[Double])
      case _: Int => termQuery(property, value)
      case _: String => comparativeQuery(op, property, value.asInstanceOf[String])
      case _ => throw InvalidFormatException("There's a problem with your query, please check the documentation.")
    }
  }

  def getSpine(cactus: Cactus): QueryDefinition = {
    if (Operation.isLogical(cactus.op) && cactus.args.isInstanceOf[List[Any]]) {
      val listArgs = cactus.args.asInstanceOf[List[Any]]

      if (listArgs.length < 2) throw InvalidFormatException("There's a problem with your query, please check the documentation.")

      if (Operation.withName(cactus.op) == AND) {
        bool {
          must(listArgs.map(x => getSpine(mapToCactus(x.asInstanceOf[Map[String, Any]]))))
        }
      } else {
        bool {
          should(listArgs.map(x => getSpine(mapToCactus(x.asInstanceOf[Map[String, Any]]))))
        }
      }
    }
    else if (Operation.isComparative(cactus.op)) {
      cactus.field match {
        case x if x.get.startsWith("details.") => {
          val name = cactus.field.get.split("\\.")(1)
          val valueType = cactus.args match {
            case _: Boolean => "value_bool"
            case _: Double => "value_float"
            case _: Int => "value_int"
            case _: String => "value_str"
            case _ => throw InvalidFormatException("There's a problem with your query, please check the documentation.")
          }
          val operation = Operation.withName(cactus.op)
          if (operation == NE) {
            nestedQuery("details").query {
              bool {
                must(termQuery("details.name", name))
                not(comparativeQuery(operation, s"details.$valueType", cactus.args))
              }
            }
          } else {
            nestedQuery("details").query {
              bool {
                must (
                  termQuery("details.name", name),
                  comparativeQuery(operation, s"details.$valueType", cactus.args)
                )
              }
            }
          }
        }
        case x if x.get == "tags" && cactus.args.isInstanceOf[List[String]] =>
          termQuery(cactus.field.get, cactus.args.asInstanceOf[String])
        case x if x.get == "category" && cactus.args.isInstanceOf[List[String]] =>
          termQuery(cactus.field.get, cactus.args.asInstanceOf[String])
        case _ => throw InvalidFormatException("There's a problem with your query, please check the documentation.")
      }
    }
    else if (Operation.isContains(cactus.op)) {
      val field = if (cactus.field.isDefined) cactus.field.get else throw InvalidFormatException("There's a problem with your query, please check the documentation.")
      val listArgs = cactus.args.asInstanceOf[List[String]]
      if (Operation.withName(cactus.op) == ALL) {
        nestedQuery("details").query {
          bool {
            must(listArgs.map(x => termQuery(s"$field.id", x)))
          }
        }
      }else {
        nestedQuery("details").query {
          bool {
            bool {
              should(listArgs.map(x => termQuery(s"$field.id", x)))
            }
          }
        }
      }
    }
    else {
      throw InvalidFormatException("There's a problem with your query, please check the documentation.")
    }
  }

  def cactusToES(jsonValue: JValue): QueryDefinition = {
    implicit val formats = DefaultFormats

    val whole = jsonValue.extract[Cactus]
    getSpine(whole)
  }
}
