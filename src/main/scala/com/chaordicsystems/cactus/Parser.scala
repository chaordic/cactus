package com.chaordicsystems.cactus

import com.chaordicsystems.cactus.Operator._
import com.sksamuel.elastic4s._
import org.json4s._
import org.json4s.jackson.JsonMethods._

case class InvalidCactusQueryFormatException(m: String = "There's a problem with your query, please check the documentation.") extends Exception

object Parser {
  implicit val formats = DefaultFormats

  def handleUnary(operation: JValue, typeEnabled: Boolean): QueryDefinition = {
    val op = Operator.withName((operation \ "op").extract[String])
    val field = Field((operation\"field").extract[String])
    val args = (operation \ "args").extract[Any]
    op match {
      case Operator.NE => field NE (args, typeEnabled)
      case Operator.EQ => field EQ (args, typeEnabled)
      case Operator.LT => field LT (args, typeEnabled)
      case Operator.GT => field GT (args, typeEnabled)
      case Operator.LE => field LE (args, typeEnabled)
      case Operator.GE => field GE (args, typeEnabled)
    }
  }

  def handleBinary(operation: JValue, typeEnabled: Boolean): QueryDefinition = {
    val op = Operator.withName((operation \ "op").extract[String])
    val args = (operation \ "args").extract[List[JValue]]

    if (args.length < 2) throw InvalidCactusQueryFormatException()

    op match {
      case Operator.AND => AND(args.map(validateAndTranslate(_, typeEnabled)))
      case Operator.OR => OR(args.map(validateAndTranslate(_, typeEnabled)))
    }
  }

  def handleMultiary(operation: JValue): QueryDefinition = {
    val op = Operator.withName((operation \ "op").extract[String])
    val args = (operation \ "args").extract[List[Any]]
    val field = Field((operation\"field").extract[String])

    op match {
      case Operator.ALL => field ALL args
      case Operator.ANY => field ANY args
    }
  }

  def validateAndTranslate(operation: JValue, typeEnabled: Boolean): QueryDefinition = {
    val op = Operator.withName((operation \ "op").extract[String])
    if (isBinary(op)) {
      handleBinary(operation, typeEnabled)
    }
    else if (isUnary(op)) {
      handleUnary(operation, typeEnabled)
    }
    else if (isMultiary(op)) {
      handleMultiary(operation)
    }
    else {
      throw InvalidCactusQueryFormatException()
    }
  }

  def cactusToES(jsonValue: String, typeEnabled: Boolean = false): QueryDefinition = validateAndTranslate(parse(jsonValue), typeEnabled)
}
