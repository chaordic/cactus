package com.chaordicsystems.cactus

import com.chaordicsystems.cactus.Operator._
import org.json4s.JsonAST.JNothing
import org.json4s.{DefaultFormats, JValue}

import scala.util.{Failure, Success, Try}

object Validator {
  case class OperatorNotProvidedException(m: String = "Operator was not provided.") extends Exception(m)
  case class InvalidOperatorException(op: Any) extends Exception(s"$op is not a valid operator.")
  case class OperatorHandlerException(op: Operator) extends Exception(s"The operator ${op.toString} wasn't handled as expected.")

  case class FieldNotProvidedException(op: String) extends Exception(s"Operator $op requires a Field and it was not provided.")
  case class InvalidFieldException(field: Any) extends Exception(s"$field is not a valid field. Please check the project specifications.")

  case class ArgsNotProvidedException(op: String) extends Exception(s"Operator $op requires a Args was not provided.")
  case class InvalidArgsException(args: Any) extends Exception(s"$args is not a valid args.")

  case class NotEnoughOperationsException(op: String) extends Exception(s"Operator $op should contain at least two other operations.")

  case class InvalidValueTypeException(x: Any) extends Exception(s"${x.getClass} is not supported. The supported types are: String, Int, Boolean and Float.")
  case class InvalidUseCaseWithTypeException(field: String) extends Exception(s"The type enabled mode needs to be done in a nested field $field.")

  implicit val formats = DefaultFormats

  implicit class JValueExtended(value: JValue) {
    def has(childString: String): Boolean = (value \ childString) != JNothing
  }

  def validateOperator(json: JValue): Operator = {
    if (json.has("op")) {
      val op = (json \ "op").extractOpt[String]
      op match {
        case Some(o) =>
          Try(Operator.withName(o)) match {
            case Success(v) => v
            case Failure(_) => throw InvalidOperatorException(o)
          }
        case _ => throw InvalidOperatorException((json \ "op").extract[Any])
      }
    } else {
      throw OperatorNotProvidedException()
    }

  }

  def validateField(json: JValue): Field = {
    if (json.has("field")) {
      val field = (json \ "field").extractOpt[String]
      field match {
        case Some(f) => Field(f)
        case _       => throw InvalidFieldException((json \ "field").extract[Any])
      }
    } else {
      throw FieldNotProvidedException((json \ "op").extract[String])
    }
  }

  def validateUnaryArgs(json: JValue): Any = {
    if (json.has("args")) {
      val args = (json \ "args").extractOpt[Any]
      args match {
        case Some(a) => a
        case _       => throw InvalidArgsException((json \ "args").extract[Any])
      }
    } else {
      throw ArgsNotProvidedException((json \ "op").extract[String])
    }
  }

  def validateBinaryArgs(json: JValue) : List[JValue] = {
    if (json.has("args")) {
      val args = (json \ "args").extractOpt[List[JValue]]
      args match {
        case Some(a) =>
          if (a.length >= 2) a
          else throw NotEnoughOperationsException((json \ "op").extract[String])
        case _ => throw InvalidArgsException((json \ "args").extract[Any])
      }
    } else {
      throw ArgsNotProvidedException((json \ "op").extract[String])
    }
  }


  def validateMultiaryArgs(json: JValue): List[Any] = {
    if (json.has("args")) {
      val args = (json \ "args").extractOpt[List[Any]]
      args match {
        case Some(a) => a
        case _       => throw InvalidArgsException((json \ "args").extract[Any])
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
