package io.jseval

import io.jseval.TypModule.Typ
import io.jseval.Expression.BuildinModule.BuildinFn
import io.jseval.Expression.Expr

enum CompilerError(msg: String):
  case NoExpectedParser(tokens: List[Token]) extends CompilerError(s"No expected parser for ${tokens.headOption}")
  case NoExpectedInfixParser(tokens: List[Token]) extends CompilerError(s"No expected parser for ${tokens.headOption}")
  case ExpectExpression(tokens: List[Token]) extends CompilerError("ExpectExpression")
  case ExpectToken(token: Token) extends CompilerError(s"Expected Token in $token")
  case ExpectTokens(tokens: List[Token])
    extends CompilerError(s"Expected Token in $tokens")

  case ExpectIdentifer() extends CompilerError(s"Expected Identifier")
  case ExpectTypeWhenLetIsRecursive(expr: Expr) extends CompilerError(s"Should define function type when it is recursive $expr")

  case ExpectClosing(tokens: List[Token])
    extends CompilerError("Expect ')' after expression")

  case WrongType(v: Any, expectedType: String) extends CompilerError(expectedType)
  case UnboundedName(token: Token) extends CompilerError(s"UnboundedName $token")
  
  case UnboundedNameWhileTypeCheck(token: Token) extends CompilerError(s"UnboundedName $token")
  
  case MissingTypeAnnotation(token: Token) extends CompilerError(s"MissingTypeAnnotation")
  case ArithmeticTypeException(leftType: Typ, rightType: Typ) extends CompilerError(s"Type mismatch")
  case ComparisonTypeException(leftType: Typ, rightType: Typ) extends CompilerError(s"Type mismatch")
  case UnaryTypeException(fn: BuildinFn.UnaryFn, exprType: Typ) extends CompilerError(s"Type mismatch")
  case IncorrectType(expectedType: String, expr: Expr, givenType: Typ) extends CompilerError(s"Type mismatch: $expr expected $expectedType, but given $givenType")


enum TypeError(msg: String) :
  case WrongType(expectedType: Typ, found: Typ) extends TypeError(s"Expected $expectedType but found $found")

