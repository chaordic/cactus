package com.chaordicsystems.cactus

import com.chaordicsystems.cactus.Operator._
import com.chaordicsystems.cactus.Validator._
import com.sksamuel.elastic4s.ElasticDsl.{bool, must, should}
import com.sksamuel.elastic4s._
import org.json4s._
import org.json4s.jackson.JsonMethods._

object Parser {
  implicit val formats = DefaultFormats

  def AND(args: List[QueryDefinition]): QueryDefinition = bool { must(args) }

  def OR(args: List[QueryDefinition]): QueryDefinition = bool { should(args) minimumShouldMatch 1 }

  def handleUnary(operation: JValue, typeEnabled: Boolean): QueryDefinition = {
    val (op, field, args) = validateUnaryOperation(operation)
    op match {
      case Operator.NE => field NE (args, typeEnabled)
      case Operator.EQ => field EQ (args, typeEnabled)
      case Operator.LT => field LT (args, typeEnabled)
      case Operator.GT => field GT (args, typeEnabled)
      case Operator.LE => field LE (args, typeEnabled)
      case Operator.GE => field GE (args, typeEnabled)
      case _           => throw OperatorHandlerException(op)
    }
  }

  def handleBinary(operation: JValue, typeEnabled: Boolean): QueryDefinition = {
    val op = validateOperator(operation)
    val args = validateBinaryArgs(operation)
    op match {
      case Operator.AND => AND(args.map(validateAndTranslate(_, typeEnabled)))
      case Operator.OR  => OR(args.map(validateAndTranslate(_, typeEnabled)))
      case _            => throw OperatorHandlerException(op)
    }
  }

  def handleMultiary(operation: JValue): QueryDefinition = {
    val (op, field, args) = validateMultiaryOperation(operation)
    op match {
      case Operator.ALL => field ALL args
      case Operator.ANY => field ANY args
      case _            => throw OperatorHandlerException(op)
    }
  }

  def validateAndTranslate(operation: JValue, typeEnabled: Boolean): QueryDefinition = {
    val op = validateOperator(operation)
    if (isBinary(op)) {
      handleBinary(operation, typeEnabled)
    } else if (isUnary(op)) {
      handleUnary(operation, typeEnabled)
    } else if (isMultiary(op)) {
      handleMultiary(operation)
    } else {
      throw InvalidOperatorException(op)
    }
  }

  def cactusToES(jsonValue: String, typeEnabled: Boolean = false): QueryDefinition = validateAndTranslate(parse(jsonValue), typeEnabled)
}
