package com.chaordicsystems.cactus

import com.chaordicsystems.cactus.Parser._
import com.sksamuel.elastic4s.ElasticDsl._
import org.json4s._
import org.json4s.jackson.JsonMethods._
import org.scalatest.WordSpec

class ParserSpec extends WordSpec {

  val query1 =
    """
      |{
      |    "op": "AND",
      |    "args": [
      |        {
                    "op":"EQ",
      |             "field": "details.tamanho",
      |             "args": "38"
      |         },
      |         {
      |             "op":"EQ",
      |             "field": "details.Peso",
      |             "args": "0.923"
      |         }
      |    ]
      |}
    """.stripMargin

  val result1 =
    """
      |{
      |  "bool": {
      |    "must": [
      |      {
      |        "nested": {
      |          "query": {
      |            "bool": {
      |              "must": [
      |                {
      |                  "term": {
      |                    "details.name": "tamanho"
      |                  }
      |                },
      |                {
      |                  "term": {
      |                    "details.value_str": "38"
      |                  }
      |                }
      |              ]
      |            }
      |          },
      |          "path": "details"
      |        }
      |      },
      |      {
      |        "nested": {
      |          "path": "details",
      |          "query": {
      |            "bool": {
      |              "must": [
      |                {
      |                  "term": {
      |                    "details.name": "Peso"
      |                  }
      |                },
      |                {
      |                  "term": {
      |                    "details.value_str": "0.923"
      |                  }
      |                }
      |              ]
      |            }
      |          },
      |          "path": "details"
      |        }
      |      }
      |    ]
      |  }
      |}
    """.stripMargin

  val query2 =
    """
      |{
      |    "op": "AND",
      |    "args": [
      |        {
      |            "op":"OR",
      |            "args":[
      |                {
      |                    "op":"EQ",
      |                    "field": "details.marca",
      |                    "args": "nike"
      |                },
      |                {
      |                    "op":"EQ",
      |                    "field": "details.size",
      |                    "args": "M"
      |                }
      |            ]
      |        },
      |        {
      |            "op":"LT",
      |            "field": "details.price",
      |            "args": 10.0
      |        }
      |    ]
      |}
    """.stripMargin

  val result2 = bool {
    must (
      bool {
      should (
        nestedQuery("details").query {
          bool {
            must (
              termQuery("details.name", "marca"),
              termQuery("details.value_str", "nike")
            )
          }
        },
        nestedQuery("details").query {
          bool {
            must (
              termQuery("details.name", "size"),
              termQuery("details.value_str", "M")
            )
          }
        }
      )
    },
      nestedQuery("details").query {
        bool {
          must (
            termQuery("details.name", "price"),
            rangeQuery("details.value_float") to 10.0
          )
        }
      }
    )
  }

  val query3 =
    """
      |{
      |    "op": "AND",
      |    "args": [
      |        {
      |          "op":"ANY",
      |          "field": "tags",
      |          "args":["adidas", "nike"]
      |        },
      |        {
      |            "op":"ALL",
      |            "field": "category",
      |            "args":["tenis"]
      |        }
      |    ]
      |}
    """.stripMargin

  "query 1" should {
    "return an ES query" in {
      implicit val formats = DefaultFormats

      val m1 = parse(cactusToES(parse(query1)).builder.toString).extract[Map[String, Any]]
      val m2 = parse(result1).extract[Map[String, Any]]

      assert((m1.toSet diff m2.toSet).toMap.isEmpty)
    }
  }

  "query 2" should {
    "return an ES query" in {
      implicit val formats = DefaultFormats

      val m1 = parse(cactusToES(parse(query2)).builder.toString).extract[Map[String, Any]]
      val m2 = parse(result2.builder.toString).extract[Map[String, Any]]

      assert((m1.toSet diff m2.toSet).toMap.isEmpty)
    }
  }
}
