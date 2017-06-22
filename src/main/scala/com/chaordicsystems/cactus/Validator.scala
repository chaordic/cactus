package com.chaordicsystems.cactus

import com.chaordicsystems.cactus.Operator._
import org.json4s.JsonAST.JNothing
import org.json4s.{DefaultFormats, JValue}

import scala.util.{Failure, Success, Try}

object Validator {
  case class OperatorNotProvidedException(m: String = "Operator was not provided. Please check the documentation.") extends Exception(m)
  case class InvalidOperatorException(op: Any) extends Exception(s"$op is not a valid operator. Please check the project specifications.")
  case class OperatorHandlerException(op: Operator) extends Exception(s"The operator ${op.toString} wasn't handled as expected.")

  case class FieldNotProvidedException(op: String) extends Exception(s"Operator $op requires a Field and it was not provided. Please check the documentation.")
  case class InvalidFieldException(field: Any) extends Exception(s"$field is not a valid field. Please check the project specifications.")

  case class ArgsNotProvidedException(op: String) extends Exception(s"Operator $op requires a Args was not provided. Please check the documentation.")
  case class InvalidArgsException(args: Any) extends Exception(s"$args is not a valid args. Please check the project specifications.")

  case class NotEnoughOperationsException(op: String) extends Exception(s"Operator $op should contain at least two other operations. Please check the documentation for more details.")

  case class InvalidValueTypeException(x: Any) extends Exception(s"${x.getClass} is not a supported. The supported types are: String, Int, Boolean and Float. Please check the documentation for more details.")
  case class InvalidUseCaseWithTypeException(field: String) extends Exception(s"The type enabled mode needs to be done in a nested field $field. Please check the documentation for more details")

  implicit val formats = DefaultFormats

  implicit class JValueExtended(value: JValue) {
    def has(childString: String): Boolean = {
      if ((value \ childString) != JNothing) {
        true
      } else {
        false
      }
    }
  }

  def validateOperator(json: JValue): Operator = {
    if (json.has("op")) {
      val op = (json \ "op").extractOpt[String]
      if (op.isDefined) {
        Try(Operator.withName(op.get)) match {
          case Success(v) => v
          case Failure(_) => throw InvalidOperatorException(op.get)
        }
      } else {
        throw InvalidOperatorException((json \ "op").extract[Any])
      }
    } else {
      throw OperatorNotProvidedException()
    }

  }

  def validateField(json: JValue): Field = {
    if (json.has("field")) {
      val field = (json \ "field").extractOpt[String]
      if (field.isDefined) {
        Field(field.get)
      } else {
        throw InvalidFieldException((json \ "field").extract[Any])
      }
    } else {
      throw FieldNotProvidedException((json \ "op").extract[String])
    }
  }

  def validateUnaryArgs(json: JValue): Any = {
    if (json.has("args")) {
      val args = (json \ "args").extractOpt[Any]
      if (args.isDefined) {
        args.get
      } else {
        throw InvalidArgsException((json \ "args").extract[Any])
      }
    } else {
      throw ArgsNotProvidedException((json \ "op").extract[String])
    }
  }

  def validateBinaryArgs(json: JValue) : List[JValue] = {
    if (json.has("args")) {
      val args = (json \ "args").extractOpt[List[JValue]]
      if (args.isDefined) {
        val result = args.get
        if (result.length < 2) throw NotEnoughOperationsException((json \ "op").extract[String])
        result
      } else {
        throw InvalidArgsException((json \ "args").extract[Any])
      }
    } else {
      throw ArgsNotProvidedException((json \ "op").extract[String])
    }
  }


  def validateMultiaryArgs(json: JValue): List[Any] = {
    if (json.has("args")) {
      val args = (json \ "args").extractOpt[List[Any]]
      if (args.isDefined) {
        args.get
      } else {
        throw InvalidArgsException((json \ "args").extract[Any])
      }
    } else {
      throw ArgsNotProvidedException((json \ "op").extract[String])
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
