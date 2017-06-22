package com.chaordicsystems.cactus

import com.chaordicsystems.cactus.Operator._
import org.json4s.{DefaultFormats, JValue}

object Validator {
  case class InvalidCactusQueryFormatException(m: String = "There's a problem with your query, please check the documentation.") extends Exception(m)
  case class OperatorNotProvidedException(m: String = "Operator was not provided, please check the documentation.") extends Exception(m)
  case class InvalidOperatorException(m: String = "That's not a valid operation. Please check the project specifications.") extends Exception(m)
  case class InvalidFieldException(m: String = "Field was not provided or is not valid. Please check the documentation.") extends Exception(m)
  case class InvalidArgsException(m: String = "Args was not provided or is not valid. Please check the documentation.") extends Exception(m)
  case class NotEnoughOperationsException(m: String = "Inside a AND or OR should contain at least two other operations, please check the documentation for more details.") extends Exception(m)
  case class InvalidValueType(m: String = "Supported types are: String, Int, Boolean and Float. Please check the documentation for more details.") extends Exception(m)

  implicit val formats = DefaultFormats

  def validateOperator(json: JValue): Operator = {
    val op = (json \ "op").extract[Option[String]]
    if (op.isDefined) {
      Operator.withName(op.get)
    } else {
      throw OperatorNotProvidedException()
    }
  }

  def validateField(json: JValue): Field = {
    val field = (json \ "field").extract[Option[String]]
    if (field.isDefined) {
      Field(field.get)
    } else {
      throw InvalidFieldException()
    }
  }

  def validateUnaryArgs(json: JValue): Any = {
    val args = (json \ "args").extract[Option[Any]]
    if (args.isDefined) {
      args.get
    } else {
      throw InvalidArgsException()
    }
  }

  def validateBinaryArgs(json: JValue) : List[JValue] = {
    val args = (json \ "args").extractOpt[List[JValue]]
    if (args.isDefined) {
      val result = args.get
      if (result.length < 2) throw NotEnoughOperationsException()
      result
    } else {
      throw InvalidArgsException()
    }
  }

  def validateMultiaryArgs(json: JValue): List[Any] = {
    val args = (json \ "args").extract[Option[List[Any]]]
    if (args.isDefined) {
      args.get
    } else {
      throw InvalidArgsException()
    }
  }

  def validateUnaryOperation(json: JValue): (Operator, Field, Any) = {
    (validateOperator(json), validateField(json), validateUnaryArgs(json))
  }

  def validateBinaryOperation(json: JValue): (Operator, Field, List[JValue]) = {
    (validateOperator(json), validateField(json), validateBinaryArgs(json))
  }

  def validateMultiaryOperation(json: JValue): (Operator, Field, List[Any]) = {
    (validateOperator(json), validateField(json), validateMultiaryArgs(json))
  }
}
