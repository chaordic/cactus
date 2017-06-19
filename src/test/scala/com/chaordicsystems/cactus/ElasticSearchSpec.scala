package com.chaordicsystems.cactus

import com.sksamuel.elastic4s.ElasticClient
import com.sksamuel.elastic4s.ElasticDsl._
import com.typesafe.config.{Config, ConfigFactory}
import org.elasticsearch.action.admin.indices.delete.DeleteIndexRequest
import org.elasticsearch.common.settings.Settings
import org.elasticsearch.node.NodeBuilder._
import org.json4s.DefaultFormats
import org.json4s.jackson.JsonMethods._
import org.scalatest.{BeforeAndAfterAll, WordSpec}
import com.chaordicsystems.cactus.CactusTests._

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.io.{BufferedSource, Source}

class ElasticSearchSpec extends WordSpec with BeforeAndAfterAll {

  val INDEX_NAME = "product_ranking"
  val DOC_TYPE = "product_ranking"

  val maxQuerySize = 50

  val config: Config = ConfigFactory.load()

  val clusterName: String = config.getString("elasticsearch.cluster.name")

  val node = Option(nodeBuilder.clusterName(clusterName)
    .settings(
      Settings.builder()
        .put("index.store.type", "mmapfs")
        .put("path.home", "/tmp")
        .build()).node().start())

  def getClient() = {
    ElasticClient.fromClient(node.get.client())
  }

  protected override def beforeAll = {
    node.foreach { n =>
      n.client.admin.indices.prepareCreate(INDEX_NAME).setSource(createMapping()).execute().actionGet()

      addProductRankings(ElasticClient.fromClient(n.client()))
    }
  }

  protected override def afterAll = {
    node.foreach { n =>
      n.client().admin().indices().delete(new DeleteIndexRequest(INDEX_NAME)).actionGet()
      n.close()
    }
  }

  def addProductRankings(client: ElasticClient): Unit = {

    implicit val format = DefaultFormats

    val products = parse(Source.fromURL(getClass.getResource("/product_rankings.json")).mkString)

    import com.sksamuel.elastic4s.jackson.ElasticJackson.Implicits._
    Await.result(
      client.execute {
        bulk(
          products.children.map { product =>
            index into INDEX_NAME -> DOC_TYPE source product.extract[Map[String,Any]] id (product \ "productId").extract[String]
          }
        )
      }, 10 seconds)

    Await.ready(
      client.execute {
        refresh index INDEX_NAME
      }, 2 seconds)
  }

  def createMapping(): String = {
    val source: BufferedSource = Source.fromURL(getClass.getResource("/product_ranking_mapping.json"))
    val mapping = source.mkString
    source.close()
    mapping
  }

  "query" should {
    "return 1 document" in {

      val query = search in INDEX_NAME -> DOC_TYPE query {
        Parser.cactusToES(parse(query1))
      }
      val result = Await.result(getClient().execute {
        query.limit(maxQuerySize)
      }, 2 seconds)

      assert(result.getHits.hits.length == 1)
    }
  }

  "query2" should {
    "return 1 document" in {
      val query = search in INDEX_NAME -> DOC_TYPE query {
        Parser.cactusToES(parse(query2))
      }
      val result = Await.result(getClient().execute {
        query.limit(maxQuerySize)
      }, 2 seconds)

      assert(result.getHits.hits.length == 1)
    }
  }

  "query3" should {
    "return 1 document" in {
      val query = search in INDEX_NAME -> DOC_TYPE query {
        Parser.cactusToES(parse(query3))
      }
      val result = Await.result(getClient().execute {
        query.limit(maxQuerySize)
      }, 2 seconds)

      assert(result.getHits.hits.length == 1)
    }
  }

  "query4" should {
    "return 1 document" in {
      val query = search in INDEX_NAME -> DOC_TYPE query {
        Parser.cactusToES(parse(query4))
      }
      val result = Await.result(getClient().execute {
        query.limit(maxQuerySize)
      }, 2 seconds)

      assert(result.getHits.hits.length == 1)
    }
  }

  "query5" should {
    "not return 1 document" in {
      val query = search in INDEX_NAME -> DOC_TYPE query {
        Parser.cactusToES(parse(query5))
      }
      val result = Await.result(getClient().execute {
        query.limit(maxQuerySize)
      }, 2 seconds)

      assert(result.getHits.hits.length == 0)
    }
  }
}
