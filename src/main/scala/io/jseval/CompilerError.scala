package io.jseval

import io.jseval.TypModule.Typ

enum CompilerError(msg: String):
  case NoExpectedParser(tokens: List[Token]) extends CompilerError(s"No expected parser for ${tokens.headOption}")
  case NoExpectedInfixParser(tokens: List[Token]) extends CompilerError(s"No expected parser for ${tokens.headOption}")
  case ExpectExpression(tokens: List[Token]) extends CompilerError("ExpectExpression")
  case ExpectToken(token: Token) extends CompilerError(s"Expected Token in $token")
  case ExpectTokens(tokens: List[Token])
    extends CompilerError(s"Expected Token in $tokens")

  case ExpectIdentifer() extends CompilerError(s"Expected Identifier")

  case ExpectClosing(tokens: List[Token])
    extends CompilerError("Expect ')' after expression")

  case WrongType(v: Any, expectedType: String) extends CompilerError(expectedType)
  case UnboundedName(token: Token) extends CompilerError(s"UnboundedName $token")


enum TypeError(msg: String) :
  case WrongType(expectedType: Typ, found: Typ) extends TypeError(s"Expected $expectedType but found $found")

