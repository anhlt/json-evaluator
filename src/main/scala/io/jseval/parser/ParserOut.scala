package io.jseval.parser

import io.jseval.Expression.Expr
import io.jseval.Token


trait ParserResult[T] {
  val expr: T
  val rmn: List[Token]
}


case class ParserOut(expr: Expr, rmn: List[Token]) extends ParserResult[Expr]
