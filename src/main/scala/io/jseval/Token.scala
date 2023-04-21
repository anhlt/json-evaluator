package io.jseval

sealed trait Token:
  val lexeme: String

enum Literal(val lexeme: String) extends Token:
  case Identifier(override val lexeme: String) extends Literal(lexeme)
  case Str(override val lexeme: String) extends Literal(lexeme)
  case Number(override val lexeme: String) extends Literal(lexeme)

enum Operator(val lexeme: String) extends Token:

  // Single character token
  case LeftParenToken extends Operator("(")
  case RightParenToken extends Operator(")")
  case LeftBraceToken extends Operator("{")
  case RightBraceToken extends Operator("}")
  case LeftBracketToken extends Operator("[")
  case RightBracketToken extends Operator("]")

  case CommaToken extends Operator(",")
  case DotToken extends Operator(".")
  case MinusToken extends Operator("-")
  case PlusToken extends Operator("+")
  case SemicolonToken extends Operator(";")
  case SlashToken extends Operator("/")
  case StarToken extends Operator("*")
  case DollarToken extends Operator("$")

  // One or two character token
  case BangToken extends Operator("!")
  case ArrowToken extends Operator("->")
  case BangEqualToken extends Operator("!=")
  case EqualToken extends Operator("=")
  case EqualEqualToken extends Operator("==")
  case GreaterToken extends Operator(">")
  case GreaterEqualToken extends Operator(">=")
  case LessToken extends Operator("<")
  case LessEqualToken extends Operator("<=")

enum Keyword(val lexeme: String) extends Token:

  case AndKw extends Keyword("and")
  case OrKw extends Keyword("or")
  case Wildcard extends Keyword("_")

  case TrueKw extends Keyword("true")
  case FalseKw extends Keyword("false")
  case Unit extends Keyword("unit")

  case LetKw extends Keyword("let")
  case InKw extends Keyword("in")
  case FunKw extends Keyword("fun")
  case RecKw extends Keyword("rec")
  case IfKw extends Keyword("if")
  case ThenKw extends Keyword("then")
  case ElseKw extends Keyword("else")

  // Type
  case StringKw extends Keyword("string")
  case IntKw extends Keyword("int")
  case BooleanKw extends Keyword("bool")

enum Comment(val lexeme: String) extends Token:
  case SingleLine(override val lexeme: String) extends Comment(lexeme)
  case Block(override val lexeme: String) extends Comment(lexeme)
