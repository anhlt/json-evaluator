package io.jseval

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
  case LeftBracket extends Operator("[")
  case RightBracket extends Operator("]")

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

  case True extends Keyword("true")
  case False extends Keyword("false")
  case Null extends Keyword("NULL")

  case Select extends Keyword("SELECT")
  case Where extends Keyword("WHERE")
  case From extends Keyword("FROM")
  case Contains extends Keyword("CONTAINS")
  case And extends Keyword("AND")
  case Or extends Keyword("OR")
  case As extends Keyword("AS")

  case Create extends Keyword("CREATE")
  case Table extends Keyword("TABLE")
  case Extract extends Keyword("EXTRACT")
  case Default extends Keyword("DEFAULT")

  // Type
  case ListToken extends Keyword("LIST")
  case String extends Keyword("STRING")
  case Int extends Keyword("INT")
  case Date extends Keyword("DATE")
  case Datetime extends Keyword("DATETIME")
  case Boolean extends Keyword("BOOLEAN")

enum Comment(val lexeme: String) extends Token:
  case SingleLine(override val lexeme: String) extends Comment(lexeme)
  case Block(override val lexeme: String) extends Comment(lexeme)
