# Cactus

Cactus aims to provide a json-like query language to make it easier to users to query in ElasticSearch.
In order to do that, Cactus was designed to be a library for Rest APIs or any other use case.


For example, given an ES document like:

```json
{
  "field1": "value1",
  "field2": [
    {
      "id": "id2_1",
      "value": "value2_1"
    },
    {
      "id": "id2_2",
      "value": "value2_2"
    },
    {
      "id": "id2_3",
      "value": "value2_3"
    },
    {
      "id": "id2_4",
      "value": "value2_4"
    }
  ],
  "field3" : {
    "id": "id3",
    "name": "name3"
  },
  "field4": "value4"
}
```

You could query it like:
```json
{
	"op": "AND",
	"args": [
    {
      "op": "ANY",
      "field": "field3.name",
      "args": ["name3", "random_name"]
    },
    {
      "op": "ALL",
      "field": "field2.value",
      "args": ["value2_1", "value2_3", "value2_4"]
    }
  ]
}
```

And the lib would provide you with a [QueryDefinition](https://www.elastic.co/guide/en/elasticsearch/reference/2.4/query-dsl.html):
```scala
bool {
    must (
      nestedQuery("field3").query {
        bool {
          should (
            matchQuery("field3.name", "name3"),
            matchQuery("field3.name", "random_name")
          ) minimumShouldMatch 1
        }
      },
      bool {
        must (
          nestedQuery("field2").query {
            bool {
              must (
                matchQuery("field2.value", "value2_1")
              )
            }
          },
          nestedQuery("field2").query {
            bool {
              must (
                matchQuery("field2.value", "value2_3")
              )
            }
          },
          nestedQuery("field2").query {
            bool {
              must (
                matchQuery("field2.value", "value2_4")
              )
            }
          }
        )
      }
    )
  }
```

The project was made using scala, [json4s](http://json4s.org/) and [elastic4s](https://github.com/sksamuel/elastic4s).

And you could easily do something like:
```scala
import com.chaordicsystems.cactus.Parser
import com.sksamuel.elastic4s._

val entity =
"""
{
  "op": "AND",
  "args": [
    {
      "op": "ANY",
      "field": "field3.name",
      "args": ["name3", "random_name"]
    },
    {
      "op": "ALL",
      "field": "field2.value",
      "args": ["value2_1", "value2_3", "value2_4"]
    }
  ]
}
"""

val query = search in "indexName" -> "documentType" query {
  Parser.cactusToES(entity)
}

client.execute {
  query
}
```
