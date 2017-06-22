package com.chaordicsystems.cactus

object Operator extends Enumeration {
  type Operator = Value
  val AND, OR, LT, GT, EQ, NE, LE, GE, ALL, ANY = Value

  val Unary: List[Operator] = List(LT, GT, EQ, NE, LE, GE)
  def isUnary(op: Operator): Boolean = Unary.contains(op)

  val Binary: List[Operator] = List(AND, OR)
  def isBinary(op: Operator): Boolean = Binary.contains(op)

  val Multiary: List[Operator] = List(ALL, ANY)
  def isMultiary(op: Operator): Boolean = Multiary.contains(op)
}


