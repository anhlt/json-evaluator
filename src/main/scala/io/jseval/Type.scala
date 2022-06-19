package io.jseval

enum Type:
  case DoubleType
  case StringType

sealed trait Declaration {
  val typeValue: Type
}

case class VariableDeclaration(typeValue: Type) extends Declaration
