package com.chaordicsystems.cactus

import com.sksamuel.elastic4s.ElasticDsl.{bool, must, should}
import com.sksamuel.elastic4s.QueryDefinition

trait Operation {

  def EQ(args: Any, typeEnabled: Boolean = false): QueryDefinition

  def LT(args: Any, typeEnabled: Boolean): QueryDefinition

  def GT(args: Any, typeEnabled: Boolean): QueryDefinition

  def LE(args: Any, typeEnabled: Boolean): QueryDefinition

  def GE(args: Any, typeEnabled: Boolean): QueryDefinition

  def NE(args: Any, typeEnabled: Boolean): QueryDefinition

  def ALL(args: List[Any]): QueryDefinition

  def AND(args: List[QueryDefinition]): QueryDefinition = bool { must(args) }

  def OR(args: List[QueryDefinition]): QueryDefinition = bool { should(args) minimumShouldMatch 1 }
}
