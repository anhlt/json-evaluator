package io.jseval

enum Error(msg: String):
  case ExpectExpression(tokens: List[Token]) extends Error("ExpectExpression")
  case ExpectToken(token: Token) extends Error(s"Expected Token in $token")
  case ExpectTokens(tokens: List[Token])
      extends Error(s"Expected Token in $tokens")

  case ExpectIdentifer() extends Error(s"Expected Identifier")

  case ExpectClosing(tokens: List[Token])
      extends Error("Expect ')' after expression")
  case InvalidAssignmentTartget(token: Token)
      extends Error("Invalid assignment target.")

  case WrongType(v: Any, expectedType: String) extends Error(expectedType)
  case UnboundedName(token: Token) extends Error(s"UnboundedName $token")
