package com.chaordicsystems.cactus

import com.chaordicsystems.cactus.Operator._
import com.sksamuel.elastic4s._
import org.json4s._
import org.json4s.jackson.JsonMethods._

case class InvalidCactusQueryFormatException(m: String = "There's a problem with your query, please check the documentation.") extends Exception

object Parser {
  implicit val formats = DefaultFormats

  def handleLogical(operation: JValue, typeEnabled: Boolean): QueryDefinition = {
    val op = Operator.withName((operation \ "op").extract[String])
    val args = (operation \ "args").extract[List[JValue]]

    if (args.length < 2) throw InvalidCactusQueryFormatException()

    op match {
      case AND => AND(args.map(x => validateAndTranslate(x, typeEnabled)))
      case OR => OR(args.map(x => validateAndTranslate(x, typeEnabled)))
    }
  }

  def handleComparative(operation: JValue): QueryDefinition = {
    val op = Operator.withName((operation \ "op").extract[String])
    val field = Field((operation\"field").extract[String])
    val args = (operation \ "args").extract[Any]
    field * (op, args, true)
  }

  def handleContains(operation: JValue): QueryDefinition = {
    val op = Operator.withName((operation \ "op").extract[String])
    val args = (operation \ "args").extract[List[Any]]
    val field = Field((operation\"field").extract[String])

    op match {
      case ALL => field ALL args
      case ANY => field ANY args
    }
  }

  def validateAndTranslate(operation: JValue, typeEnabled: Boolean): QueryDefinition = {
    val op = Operator.withName((operation \ "op").extract[String])
    if (isBinary(op)) {
      handleLogical(operation, typeEnabled)
    }
    else if (isUnary(op)) {
      handleComparative(operation)
    }
    else if (isMultiary(op)) {
      handleContains(operation)
    }
    else {
      throw InvalidCactusQueryFormatException()
    }
  }

  def cactusToES(jsonValue: JValue): QueryDefinition = cactusToES(jsonValue, typeEnabled = false)
  def cactusToES(jsonValue: JValue, typeEnabled: Boolean): QueryDefinition = validateAndTranslate(jsonValue, typeEnabled)

  def cactusToES(jsonValue: String): QueryDefinition = cactusToES(jsonValue, typeEnabled = false)
  def cactusToES(jsonValue: String, typeEnabled: Boolean): QueryDefinition = validateAndTranslate(parse(jsonValue), typeEnabled)
}
