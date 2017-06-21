package com.chaordicsystems.cactus

import com.sksamuel.elastic4s.QueryDefinition
import com.sksamuel.elastic4s.ElasticDsl._

case class InvalidOperatorException(m: String = "That's not a valid operation. Please check the project specifications.") extends Exception

object Operator extends Enumeration {
  type Operation = Value
  val AND, OR, LT, GT, EQ, NE, LE, GE, ALL, ANY = Value

  def isLogical: Boolean = Value == AND || Value == OR
  def isLogical(s: String): Boolean = withName(s) == AND || withName(s) == OR

  def isComparative: Boolean =
    Value == LT || Value == GT || Value == EQ || Value == NE || Value == LE || Value == GE
  def isComparative(s: String): Boolean =
    withName(s) == LT || withName(s) == GT || withName(s) == EQ || withName(s) == NE || withName(s) == LE || withName(s) == GE

  def isContains: Boolean = Value == ALL || Value == ANY
  def isContains(s: String): Boolean = withName(s) == ALL || withName(s) == ANY

  def AND(args: List[QueryDefinition]): QueryDefinition = bool { must(args) }

  def OR(args: List[QueryDefinition]): QueryDefinition = bool { should(args) minimumShouldMatch 1 }

}


