package com.chaordicsystems.cactus

  object Operation extends Enumeration {
    type Operation = Value
    val AND = Value("AND")
    val OR = Value("OR")
    val LT = Value("LT")
    val GT = Value("GT")
    val EQ = Value("EQ")
    val NE = Value("NE")
    val LE = Value("LE")
    val GE = Value("GE")
    val ALL = Value("ALL")
    val ANY = Value("ANY")

    def isLogical: Boolean = Value == AND || Value == OR
    def isLogical(s: String): Boolean = withName(s) == AND || withName(s) == OR

    def isComparative: Boolean =
      Value == LT || Value == GT || Value == EQ || Value == NE || Value == LE || Value == GE
    def isComparative(s: String): Boolean =
      withName(s) == LT || withName(s) == GT || withName(s) == EQ || withName(s) == NE || withName(s) == LE || withName(s) == GE

    def isContains: Boolean = Value == ALL || Value == ANY
    def isContains(s: String): Boolean = withName(s) == ALL || withName(s) == ANY
}
