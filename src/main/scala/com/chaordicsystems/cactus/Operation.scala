package com.chaordicsystems.cactus

import com.sksamuel.elastic4s.QueryDefinition

trait UnaryOperation {
  def EQ(args: Any, typeEnabled: Boolean = false): QueryDefinition

  def LT(args: Any, typeEnabled: Boolean): QueryDefinition

  def GT(args: Any, typeEnabled: Boolean): QueryDefinition

  def LE(args: Any, typeEnabled: Boolean): QueryDefinition

  def GE(args: Any, typeEnabled: Boolean): QueryDefinition

  def NE(args: Any, typeEnabled: Boolean): QueryDefinition
}

trait BinaryOperation {
  def AND(args: List[QueryDefinition]): QueryDefinition
  def OR(args: List[QueryDefinition]): QueryDefinition
}

trait MultiaryOperation {
  def ALL(args: List[Any]): QueryDefinition
  def ANY(args: List[Any]): QueryDefinition
}
