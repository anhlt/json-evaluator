package io.jseval

sealed trait Token:
  val lexeme: String

case class ExtraToken[+A](info: A, token: Token)

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

  case And extends Keyword("and")
  case Or extends Keyword("or")
  case Wildcard extends Keyword("_")

  case True extends Keyword("true")
  case False extends Keyword("false")
  case Unit extends Keyword("unit")

  case Let extends Keyword("let")
  case In extends Keyword("in")
  case Fun extends Keyword("fun")

  // Type
  case String extends Keyword("string")
  case Int extends Keyword("int")
  case Boolean extends Keyword("bool")

enum Comment(val lexeme: String) extends Token:
  case SingleLine(override val lexeme: String) extends Comment(lexeme)
  case Block(override val lexeme: String) extends Comment(lexeme)
