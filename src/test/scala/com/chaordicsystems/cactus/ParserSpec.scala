package com.chaordicsystems.cactus

import com.chaordicsystems.cactus.CactusTests._
import com.chaordicsystems.cactus.Parser._
import com.chaordicsystems.cactus.Validator.{ArgsNotProvidedException, FieldNotProvidedException, InvalidArgsException, InvalidUseCaseWithTypeException, OperatorNotProvidedException}
import com.fasterxml.jackson.core.JsonParseException
import org.json4s._
import org.json4s.jackson.JsonMethods._
import org.scalatest.WordSpec

class ParserSpec extends WordSpec {

  implicit val formats = DefaultFormats

  "query 1" should {
    "return an ES query as shown in result 1" in {
      val m1 = parse(cactusToES(query1, typeEnabled = true).builder.toString).extract[Map[String, Any]]
      val m2 = parse(result1).extract[Map[String, Any]]

      assert((m1.toSet diff m2.toSet).toMap.isEmpty)
    }
  }

  "query 2" should {
    "return an ES query as shown in result 2" in {
      val m1 = parse(cactusToES(query2, typeEnabled = true).builder.toString).extract[Map[String, Any]]
      val m2 = parse(result2.builder.toString).extract[Map[String, Any]]

      assert((m1.toSet diff m2.toSet).toMap.isEmpty)
    }
  }

  "query 3" should {
    "return an ES query as shown in result 3" in {
      val m1 = parse(cactusToES(query3, typeEnabled = true).builder.toString).extract[Map[String, Any]]
      val m2 = parse(result3.builder.toString).extract[Map[String, Any]]

      assert((m1.toSet diff m2.toSet).toMap.isEmpty)
    }
  }

  "query 4" should {
    "return an ES query as shown in result 4" in {
      val m1 = parse(cactusToES(query4, typeEnabled = true).builder.toString).extract[Map[String, Any]]
      val m2 = parse(result4.builder.toString).extract[Map[String, Any]]

      assert((m1.toSet diff m2.toSet).toMap.isEmpty)
    }
  }

  "invalid json" should {
    "fail when attempting to parse it" in {
      intercept[JsonParseException] {
        cactusToES(failJsonQuery, typeEnabled = true)
      }
    }
  }

  "non cactus query" should {
    "fail when validating it, OR expects a list of Operations(JValue)" in {
      intercept[InvalidArgsException] {
        cactusToES(failQuery, typeEnabled = true)
      }
    }
  }

  "testing operators" should {
    "be parse correctly" in {
      val m1 = parse(cactusToES(UnaryOperatorsQuery).builder.toString).extract[Map[String, Any]]
      val m2 = parse(UnaryOperatorsResult.builder.toString).extract[Map[String, Any]]

      assert((m1.toSet diff m2.toSet).toMap.isEmpty)
    }
  }

  "type enabled parses" should {
    "only work on nested fields" in {
      intercept[InvalidUseCaseWithTypeException] {
        cactusToES(UnaryOperatorsQuery, typeEnabled = true)
      }
    }
  }

  "blank query" should {
    "thrown an exception" in {
      intercept[OperatorNotProvidedException] {
        cactusToES(blankQuery)
      }
    }
  }

  "non op query" should {
    "thrown an exceptions" in {
      intercept[OperatorNotProvidedException] {
        cactusToES(nonOpQuery)
      }
    }
  }

  "non field query" should {
    "thrown an exception" in {
      intercept[FieldNotProvidedException] {
        cactusToES(nonFieldQuery)
      }
    }
  }

  "non args query" should {
    "thrown an exception" in {
      intercept[ArgsNotProvidedException] {
        cactusToES(nonArgsQuery)
      }
    }
  }
}
