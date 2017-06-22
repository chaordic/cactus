package com.chaordicsystems.cactus

import com.sksamuel.elastic4s.ElasticDsl._
import com.sksamuel.elastic4s.QueryDefinition

object CactusTests {

  val query1: String =
    """
      |{
      |    "op": "AND",
      |    "args": [
      |        {
      |             "op":"EQ",
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

  val result1: String =
    """
      |{
      |  "bool" : {
      |    "must" : [ {
      |      "nested" : {
      |        "query" : {
      |          "bool" : {
      |            "must" : [ {
      |              "match" : {
      |                "details.name" : {
      |                  "query" : "tamanho",
      |                  "type" : "boolean"
      |                }
      |              }
      |            }, {
      |              "match" : {
      |                "details.value_str" : {
      |                  "query" : "38",
      |                  "type" : "boolean"
      |                }
      |              }
      |            } ]
      |          }
      |        },
      |        "path" : "details"
      |      }
      |    }, {
      |      "nested" : {
      |        "query" : {
      |          "bool" : {
      |            "must" : [ {
      |              "match" : {
      |                "details.name" : {
      |                  "query" : "Peso",
      |                  "type" : "boolean"
      |                }
      |              }
      |            }, {
      |              "match" : {
      |                "details.value_str" : {
      |                  "query" : "0.923",
      |                  "type" : "boolean"
      |                }
      |              }
      |            } ]
      |          }
      |        },
      |        "path" : "details"
      |      }
      |    } ]
      |  }
      |}
    """.stripMargin

  val query2: String =
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

  val result2: QueryDefinition = bool {
    must(
      bool {
        should(
          nestedQuery("details").query {
            bool {
              must(
                matchQuery("details.name", "marca"),
                matchQuery("details.value_str", "nike")
              )
            }
          },
          nestedQuery("details").query {
            bool {
              must(
                matchQuery("details.name", "size"),
                matchQuery("details.value_str", "M")
              )
            }
          }
        ) minimumShouldMatch 1
      },
      nestedQuery("details").query {
        bool {
          must(
            matchQuery("details.name", "price"),
            rangeQuery("details.value_float") to 10.0 includeUpper false
          )
        }
      }
    )
  }

  val query3: String =
    """
      |{
      |    "op": "AND",
      |    "args": [
      |        {
      |          "op":"ANY",
      |          "field": "tags.id",
      |          "args":["adidas", "nike"]
      |        },
      |        {
      |            "op":"ALL",
      |            "field": "categories.id",
      |            "args":["tenis"]
      |        }
      |    ]
      |}
    """.stripMargin

  val result3: QueryDefinition = bool {
    must(
      nestedQuery("tags").query {
        bool {
          should(
            matchQuery("tags.id", "adidas"),
            matchQuery("tags.id", "nike")
          ) minimumShouldMatch 1
        }
      },
      nestedQuery("categories").query {
        bool {
          must(
            matchQuery("categories.id", "tenis")
          )
        }
      }
    )
  }

  val query4: String =
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
      |                    "field": "categories.id",
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

  val result4: QueryDefinition = bool {
    must(
      should(
        nestedQuery("details").query {
          bool {
            must(
              matchQuery("details.name", "available"),
              termQuery("details.value_bool", true)
            )
          }
        },
        nestedQuery("details").query {
          bool {
            must(
              matchQuery("details.name", "size"),
              rangeQuery("details.value_int") from 10 includeLower true includeUpper true
            )
          }
        }
      ) minimumShouldMatch 1,
      must(
        bool {
          must(
            nestedQuery("categories").query {
              bool {
                must(
                  matchQuery("categories.id", "shoes")
                )
              }
            },
            nestedQuery("categories").query {
              bool {
                must(
                  matchQuery("categories.id", "adidas")
                )
              }
            }
          )
        },
        nestedQuery("details").query {
          bool {
            must(
              matchQuery("details.name", "price"),
              matchQuery("details.value_float", 10.98)
            )
          }
        }
      )
    )
  }

  val query5: String =
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
      |                    "field": "categories.id",
      |                    "args": ["preto", "olympus"]
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


  val failQuery: String =
    """
      |{
      |    "op": "AND",
      |    "args": [
      |        {
      |             "op":"EQ",
      |             "field": "details.tamanho",
      |             "args": "38"
      |         },
      |         {
      |             "op":"OR",
      |             "field": "details.Peso",
      |             "args": "0.923"
      |         }
      |    ]
      |}
    """.stripMargin


  val failJsonQuery: String =
    """
      |{
      |    "op": "AND",
      |    "args": [
      |        {
      |             "op":"EQ",
      |             "field": "details.tamanho",
      |             "args": "38"
      |         }
      |         {
      |             "op":"EQ",
      |             "field": "details.Peso",
      |             "args": "0.923"
      |         }
      |    ]
      |}
    """.stripMargin


  val UnaryOperatorsQuery: String =
    """
      |{
      |    "op": "AND",
      |    "args": [
      |        {
      |             "op":"EQ",
      |             "field": "oh.baby",
      |             "args": "equality"
      |         },
      |         {
      |              "op":"NE",
      |              "field": "something",
      |              "args": "not_right"
      |         },
      |         {
      |             "op":"LT",
      |             "field": "size",
      |             "args": 10
      |         },
      |         {
      |             "op":"LE",
      |             "field": "size",
      |             "args": 10
      |         },
      |         {
      |             "op":"GT",
      |             "field": "shoes.price",
      |             "args": 42.0
      |         },
      |         {
      |             "op":"GE",
      |             "field": "shoes.price",
      |             "args": 42.0
      |         }
      |    ]
      |}
    """.stripMargin


  val UnaryOperatorsResult: QueryDefinition = bool {
    must(
      nestedQuery("oh").query {
        bool {
          must (
            matchQuery("oh.baby", "equality")
          )
        }
      },
      not{ matchQuery("something", "not_right") },
      must(rangeQuery("size") to 10 includeUpper false),
      must(rangeQuery("size") to 10 includeUpper true),
      nestedQuery("shoes").query {
        bool {
          must(
            rangeQuery("shoes.price") from 42.0 includeLower false includeUpper true
          )
        }
      },
      nestedQuery("shoes").query {
        bool {
          must(
            rangeQuery("shoes.price") from 42.0 includeLower true includeUpper true
          )
        }
      }
    )
  }
}
