package com.chaordicsystems.cactus

import com.sksamuel.elastic4s.QueryDefinition
import com.sksamuel.elastic4s.ElasticDsl._

case class InvalidOperatorException(m: String = "That's not a valid operation. Please check the project specifications.") extends Exception

object Operator extends Enumeration {
  type Operator = Value
  val AND, OR, LT, GT, EQ, NE, LE, GE, ALL, ANY = Value

  def isUnary(op: Operator): Boolean = op == LT || op == GT || op == EQ || op == NE || op == LE || op == GE

  def isBinary(op: Operator): Boolean = op == AND || op == OR

  def isMultiary(op: Operator): Boolean = op == ALL || op == ANY

  def AND(args: List[QueryDefinition]): QueryDefinition = bool { must(args) }

  def OR(args: List[QueryDefinition]): QueryDefinition = bool { should(args) minimumShouldMatch 1 }
}


