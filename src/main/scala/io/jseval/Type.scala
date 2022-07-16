package io.jseval

enum SupportType:
  case DoubleType
  case StringType

sealed trait Declaration {
  val typeValue: SupportType
}

case class VariableDeclaration(typeValue: SupportType) extends Declaration
