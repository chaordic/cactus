package com.chaordicsystems.cactus

object Validator {

  case class InvalidCactusQueryFormatException(m: String = "There's a problem with your query, please check the documentation.") extends Exception

}
