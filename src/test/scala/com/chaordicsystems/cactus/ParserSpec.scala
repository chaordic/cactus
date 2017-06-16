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
            rangeQuery("details.value_float") to 10.0 includeUpper false
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
      |            "field": "categories",
      |            "args":["tenis"]
      |        }
      |    ]
      |}
    """.stripMargin

  val result3 = bool {
    must (
      nestedQuery("tags").query {
        bool {
          should (
            termQuery("tags.id", "adidas"),
            termQuery("tags.id", "nike")
          )
        }
      },
      nestedQuery("categories").query {
        bool {
          must (
            termQuery("categories.id", "tenis")
          )
        }
      }
    )
  }

  val query4 =
    """
      |{
      |    "op": "AND",
      |    "args": [
      |        {
      |            "op":"OR",
      |            "args":[
      |                {
      |                    "op":"EQ",
      |                    "field": "details.available",
      |                    "args": true
      |                },
      |                {
      |                    "op":"GE",
      |                    "field": "details.size",
      |                    "args": 10
      |                }
      |            ]
      |        },
      |        {
      |            "op":"AND",
      |            "args": [
      |                {
      |                    "op":"ALL",
      |                    "field": "categories",
      |                    "args": ["shoes", "adidas"]
      |                },
      |                {
      |                    "op":"EQ",
      |                    "field": "details.price",
      |                    "args": 10.98
      |                }
      |            ]
      |        }
      |    ]
      |}
    """.stripMargin

  val result4 = bool {
    must (
      should(
        nestedQuery("details").query {
          bool {
            must (
              termQuery("details.name", "available"),
              termQuery("details.value_bool", true)
            )
          }
        },
        nestedQuery("details").query {
          bool {
            must (
              termQuery("details.name", "size"),
              rangeQuery("details.value_int") from 10 includeLower true includeUpper true
            )
          }
        }
      ),
      must(
        nestedQuery("categories").query {
          bool {
            must (
              termQuery("categories.id", "shoes"),
              termQuery("categories.id", "adidas")
            )
          }
        },
        nestedQuery("details").query {
          bool {
            must (
              termQuery("details.name", "price"),
              termQuery("details.value_float", 10.98)
            )
          }
        }
      )
    )
  }





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
