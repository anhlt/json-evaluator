package io.jseval.parser

import io.jseval.Expression.Expr
import io.jseval.Token
import io.jseval.TypModule.Typ


trait ParserResult[T] {
  val expr: T
  val rmn: List[Token]
}


case class ParserOut(expr: Expr, rmn: List[Token]) extends ParserResult[Expr]
case class TypeParserResult(expr: Typ, rmn: List[Token]) extends ParserResult[Typ]

