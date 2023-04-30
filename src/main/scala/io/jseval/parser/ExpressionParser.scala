package io.jseval.parser

import cats.MonadError
import io.jseval.parser.{ParserOut , ParserResult }
import io.jseval.{Token, CompilerError}
import cats.implicits._
import io.jseval.parser.AndInfixParser.precedence
import io.jseval.Expression.{Expr, App}
import cats.parse.Parser
import io.jseval.parser.Utils._
import io.jseval.Operator
import io.jseval.TypModule._

object ExpressionParser extends BaseParser[Expr] {


  override val baseGrammar: BaseGrammar[Expr] = Grammar

  override def constructOutPut(expr: Expr, rmn: List[Token]): ParserResult[Expr] = ParserOut(expr, rmn)

  def expression[F[_]](
      tokens: List[Token],
      precedence: Precendence = Precendence.LOWEST
  )(implicit
      a: MonadError[F, CompilerError]
  ): F[ParserResult[Expr]] =
    parsePrecedence(precedence, tokens)


}
