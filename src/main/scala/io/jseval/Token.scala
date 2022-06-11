package io.jseval

import com.fasterxml.jackson.module.scala.deser.overrides

sealed trait Token:
  val lexeme: String

enum Literal(val lexeme: String) extends Token:
  case Identifier(override val lexeme: String) extends Literal(lexeme)
  case Str(override val lexeme: String) extends Literal(lexeme)
  case Number(override val lexeme: String) extends Literal(lexeme)

enum Operator(val lexeme: String) extends Token:

  // Single character token
  case LeftParen extends Operator("(")
  case RightParen extends Operator(")")
  case LeftBrace extends Operator("{")
  case RightBrace extends Operator("}")
  case Comma extends Operator(",")
  case Dot extends Operator(".")
  case Minus extends Operator("-")
  case Plus extends Operator("+")
  case Semicolon extends Operator(";")
  case Slash extends Operator("/")
  case Star extends Operator("*")
  case Dollar extends Operator("$")

  // One or two character token
  case Bang extends Operator("!")
  case BangEqual extends Operator("!=")
  case Equal extends Operator("=")
  case EqualEqual extends Operator("==")
  case Greater extends Operator(">")
  case GreaterEqual extends Operator(">=")
  case Less extends Operator("<")
  case LessEqual extends Operator("<=")

enum Keyword(val lexeme: String) extends Token:

  case Select extends Keyword("SELECT")
  case Where extends Keyword("WHERE")
  case From extends Keyword("FROM")
  case Contains extends Keyword("CONTAINS")

  case Create extends Keyword("CREATE")
  case Table extends Keyword("TABLE")
  case Values extends Keyword("VALUES")

  // Type
  case List extends Keyword("LIST")
  case String extends Keyword("STR")
  case Int extends Keyword("INT")
  case Date extends Keyword("DATE")
  case Datetime extends Keyword("DATETIME")
