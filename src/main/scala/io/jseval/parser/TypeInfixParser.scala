package io.jseval.parser

import io.jseval.Token
import io.jseval.TypeError
import cats._
import cats.implicits._
import io.jseval.TypModule._
import io.jseval.CompilerError

trait TypeInfixParser {
  val parser: (Typ, Typ) => Typ
  val precedence: TypePrecedence

  def parseType[F[_]](tokens: List[Token], leftType: Typ)(implicit
      me: MonadError[F, CompilerError],
      jpParser : JSParser
  ): F[(Typ, List[Token])] = ??? 
}

case object ArrowInfixParser extends TypeInfixParser:
  val parser: (Typ, Typ) => Typ = (l, r) => TArrow(l, r)

  val precedence: TypePrecedence = TypePrecedence.ARROW

case object ProductInfixParser extends TypeInfixParser:
  val parser: (Typ, Typ) => Typ = (l, r) => TProduct(l, r)

  val precedence: TypePrecedence = TypePrecedence.PRODUCT
