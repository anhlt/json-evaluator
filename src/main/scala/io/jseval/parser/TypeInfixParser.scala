package io.jseval.parser

import io.jseval.Token
import io.jseval.TypeError
import cats._
import cats.implicits._
import io.jseval.TypModule._
import io.jseval.CompilerError

trait TypeInfixParser extends InfixParser[Typ] {
  val parser: (Typ, Typ) => Typ
  val precedence: Precendence

  def parse[F[_]](ts: List[Token], leftExpr: Typ)(implicit
      a: MonadError[F, CompilerError]
  ): F[ParserResult[Typ]] = {

    TypeParser.parseType(ts, precedence = precedence).map {
      rightExprAndRemaining =>
        TypeParserResult(
          parser(leftExpr, rightExprAndRemaining.expr),
          rightExprAndRemaining.rmn
        )
    }
  }
}

case object ArrowInfixParser extends TypeInfixParser:
  val parser: (Typ, Typ) => Typ = (l, r) => TArrow(l, r)

  val precedence: Precendence = Precendence.ARROW

  override def parse[F[_]](ts: List[Token], leftExpr: Typ)(implicit
      a: MonadError[F, CompilerError]
  ): F[ParserResult[Typ]] = {

    TypeParser.parsePrecedence(Precendence.ASSIGNMENT, ts).map {
      rightExprAndRemaining =>
        TypeParserResult(
          parser(leftExpr, rightExprAndRemaining.expr),
          rightExprAndRemaining.rmn
        )
    }
  }


case object ProductInfixParser extends TypeInfixParser:
  val parser: (Typ, Typ) => Typ = (l, r) => TProduct(l, r)

  val precedence: Precendence = Precendence.PRODUCT

  override def parse[F[_]](ts: List[Token], leftExpr: Typ)(implicit
      a: MonadError[F, CompilerError]
  ): F[ParserResult[Typ]] = {

    TypeParser.parsePrecedence(Precendence.ARROW, ts).map {
      rightExprAndRemaining =>
        TypeParserResult(
          parser(leftExpr, rightExprAndRemaining.expr),
          rightExprAndRemaining.rmn
        )
    }
  }

