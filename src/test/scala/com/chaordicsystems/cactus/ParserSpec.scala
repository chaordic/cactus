package com.chaordicsystems.cactus

import com.chaordicsystems.cactus.Parser._
import org.json4s._
import org.json4s.jackson.JsonMethods._
import org.scalatest.WordSpec
import com.chaordicsystems.cactus.Spines._

class ParserSpec extends WordSpec {
  "query 1" should {
    "return an ES query  as shown in result 1" in {
      implicit val formats = DefaultFormats

      val m1 = parse(cactusToES(parse(query1)).builder.toString).extract[Map[String, Any]]
      val m2 = parse(result1).extract[Map[String, Any]]

      assert((m1.toSet diff m2.toSet).toMap.isEmpty)
    }
  }

  "query 2" should {
    "return an ES query as shown in result 2" in {
      implicit val formats = DefaultFormats

      val m1 = parse(cactusToES(parse(query2)).builder.toString).extract[Map[String, Any]]
      val m2 = parse(result2.builder.toString).extract[Map[String, Any]]

      assert((m1.toSet diff m2.toSet).toMap.isEmpty)
    }
  }

  "query 3" should {
    "return an ES query as shown in result 3" in {
      implicit val formats = DefaultFormats

      val m1 = parse(cactusToES(parse(query3)).builder.toString).extract[Map[String, Any]]
      val m2 = parse(result3.builder.toString).extract[Map[String, Any]]

      assert((m1.toSet diff m2.toSet).toMap.isEmpty)
    }
  }

  "query 4" should {
    "return an ES query as shown in result 4" in {
      implicit val formats = DefaultFormats

      val m1 = parse(cactusToES(parse(query4)).builder.toString).extract[Map[String, Any]]
      val m2 = parse(result4.builder.toString).extract[Map[String, Any]]

      println(cactusToES(parse(query4)).builder)
      println(result4.builder)

      assert((m1.toSet diff m2.toSet).toMap.isEmpty)
    }
  }
}
