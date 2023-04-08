package io.jseval.parser

import cats.MonadError
import io.jseval.Parser.ParserOut
import io.jseval.Token
import io.jseval.CompilerError
import io.jseval.Literal
import io.jseval.Expression
import io.jseval.Keyword
import io.jseval.Operator
import cats.implicits._
import io.jseval.Expression.BuildinModule.BuildinFn
import io.jseval.Expression.Buildin

trait PrefixParser {
  def parse[F[_]](
      tokens: List[Token]
  )(implicit a: MonadError[F, CompilerError], JSParser: JSParser): F[ParserOut]
}

case class LiteralParser() extends PrefixParser {

  def parse[F[_]](tokens: List[Token])(implicit
      a: MonadError[F, CompilerError],
      JSParser: JSParser
  ): F[ParserOut] = {
    tokens match
      case Literal.Number(l) :: rest =>
        a.pure(ParserOut(Expression.LiteralExpr(l.toDouble), rest))
      case Literal.Str(l) :: rest =>
        a.pure(ParserOut(Expression.LiteralExpr(l), rest))
      case Keyword.True :: rest =>
        a.pure(ParserOut(Expression.LiteralExpr(true), rest))
      case Keyword.False :: rest =>
        a.pure(ParserOut(Expression.LiteralExpr(false), rest))
      case _ => a.raiseError(CompilerError.ExpectExpression(tokens))
  }

}

case class IdentifierParser() extends PrefixParser {
  def parse[F[_]](tokens: List[Token])(implicit
      a: MonadError[F, CompilerError],
      JSParser: JSParser
  ): F[ParserOut] = {
    tokens match
      case Literal.Identifier(name) :: rest =>
        a.pure(ParserOut(Expression.Variable(Literal.Identifier(name)), rest))
      case _ => a.raiseError(CompilerError.ExpectExpression(tokens))
  }
}

case class UnaryPrefixParser() extends PrefixParser {
  def parse[F[_]](tokens: List[Token])(implicit
      a: MonadError[F, CompilerError],
      JSParser: JSParser
  ): F[ParserOut] = {
    tokens match
      case Operator.Minus :: rest =>
        for {
          parserOut <- JSParser.parsePrecedence(Precendence.UNARY, rest)
        } yield ParserOut(
          Buildin(BuildinFn.Unary(BuildinFn.Negate, parserOut.expr)),
          parserOut.rmn
        )
      case Operator.Bang :: rest =>
        for {
          parserOut <- JSParser.parsePrecedence(Precendence.UNARY, rest)
        } yield ParserOut(
          Buildin(BuildinFn.Unary(BuildinFn.Not, parserOut.expr)),
          parserOut.rmn
        )
      case _ => a.raiseError(CompilerError.ExpectExpression(tokens))
  }
}

case class ParenthesisParser() extends PrefixParser {
  def parse[F[_]](tokens: List[Token])(implicit
      a: MonadError[F, CompilerError],
      JSParser: JSParser
  ): F[ParserOut] = {

    tokens match
      case Operator.LeftParen :: rest =>
        parenBody(rest, JSParser)
      case _ => a.raiseError(CompilerError.ExpectExpression(tokens))
  }
  // Parse the body within a pair of parentheses (the part after "(")
  def parenBody[F[_]](
      tokens: List[Token],
      JSParser: JSParser
  )(implicit a: MonadError[F, CompilerError]): F[ParserOut] =
    for {
      parserOut <- JSParser.expression(tokens)
      rmn = parserOut.rmn
      result <- rmn match
        case Operator.RightParen :: rmn =>
          a.pure(ParserOut(Expression.Grouping(parserOut.expr), rmn))
        case _ => a.raiseError(CompilerError.ExpectClosing(rmn))
    } yield (result)

}

case class ConditionPrefixParser()  extends PrefixParser {
  def parse[F[_]](tokens: List[Token])(implicit
      a: MonadError[F, CompilerError],
      JSParser: JSParser
  ): F[ParserOut] = {
    tokens match
      case Keyword.If :: rest =>
        for {
          parserOut <- JSParser.expression(rest)
          rmn = parserOut.rmn
          result <- rmn match
            case Keyword.Then :: rmn =>
              for {
                parserOut2 <- JSParser.expression(rmn)
                rmn2 = parserOut2.rmn
                result <- rmn2 match
                  case Keyword.Else :: rmn2 =>
                    for {
                      parserOut3 <- JSParser.expression(rmn2)
                      rmn3 = parserOut3.rmn
                    } yield ParserOut(
                      Expression.Cond(
                        parserOut.expr,
                        parserOut2.expr,
                        parserOut3.expr
                      ),
                      rmn3
                    )
                  case _ => a.raiseError(CompilerError.ExpectToken(Keyword.Then))
              } yield (result)
            case _ => a.raiseError(CompilerError.ExpectToken(Keyword.Else))
        } yield (result)
      case _ => a.raiseError(CompilerError.ExpectExpression(tokens))
  }
}


case class BracketPrefixParser() extends PrefixParser {
  def parse[F[_]](tokens: List[Token])(implicit
      a: MonadError[F, CompilerError],
      JSParser: JSParser
  ): F[ParserOut] = ???
}

case class BracePrefixParser() extends PrefixParser {
  def parse[F[_]](tokens: List[Token])(implicit
      a: MonadError[F, CompilerError],
      JSParser: JSParser
  ): F[ParserOut] = ???
}
