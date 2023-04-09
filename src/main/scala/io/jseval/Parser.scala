package io.jseval

import cats._
import cats.implicits._
import cats.instances._
import io.jseval.Expression
import io.jseval.Expression.BuildinModule.BuildinFn
import io.jseval.TypModule._

import Expression._

object Parser {

  def parseExpr[F[_]](f: List[Token] => F[ParserOut])(tokens: List[Token])(
      implicit a: MonadError[F, CompilerError]
  ): F[ParserOut] = for {
    rs <- f(tokens)
    rs2 <- consume(Operator.Semicolon, rs._2)
  } yield ParserOut(rs._1, rs2._2)

  def expression[F[_]](
      tokens: List[Token]
  )(implicit a: MonadError[F, CompilerError]): F[ParserOut] =
    let(tokens)
      .orElse(condition(tokens))
      .orElse(lambdaFunc(tokens))
      .orElse(or(tokens))
      .orElse(app(tokens))

  // Let x = 5
  // Let y = 6
  // Let sum = fun g h -> g + h
  // in sum (x, y)

  def condition[F[_]](
      tokens: List[Token]
  )(implicit me: MonadError[F, CompilerError]): F[ParserOut] = {

    for {
      ifAndRmn <- consume(expect = Keyword.If, tokens = tokens)
      (_, afterIf) = ifAndRmn
      condAndRmn <- expression(afterIf)
      thenAndRmn <- consume(Keyword.Then, condAndRmn.rmn)
      trueExprAndRmn <- expression(thenAndRmn._2)
      elseAndRmn <- consume(Keyword.Else, trueExprAndRmn.rmn)
      falseExprAndRmn <- expression(elseAndRmn._2)

    } yield ParserOut(
      Cond(
        condAndRmn.expr,
        trueBranch = trueExprAndRmn.expr,
        falseBranch = falseExprAndRmn.expr
      ),
      falseExprAndRmn.rmn
    )
  }

  def let[F[_]](
      tokens: List[Token]
  )(implicit me: MonadError[F, CompilerError]): F[ParserOut] = {

    for {
      letAndRmn <- consume(Keyword.Let, tokens)
      (letKw, rmn) = letAndRmn
      isRecursive <- rec(rmn)
      (isRec, afterRec) = isRecursive
      identiferAndRmn <- identifier(afterRec)
      variable <- asVariable(identiferAndRmn.expr)
      equalAndRmn <- consume(Operator.Equal, identiferAndRmn.rmn)
      exprAndRmn <- expression(equalAndRmn._2)
      result <- (for {
        rs <- in(exprAndRmn.rmn)
      } yield rs).recoverWith({ case CompilerError.ExpectToken(Keyword.In) =>
        let(exprAndRmn.rmn)
      })

    } yield ParserOut(
      Binding(isRec, variable, exprAndRmn.expr, result._1),
      result._2
    )
  }

  def rec[F[_]](
      tokens: List[Token]
  )(implicit a: MonadError[F, CompilerError]): F[(Boolean, List[Token])] =
    tokens.headOption match {
      case Some(Keyword.Rec) => a.pure(true, tokens.tail)
      case _                 => a.pure(false, tokens)
    }

  def in[F[_]](
      tokens: List[Token]
  )(implicit me: MonadError[F, CompilerError]): F[ParserOut] = {

    for {
      inAndRmn <- consume(Keyword.In, tokens)
      (_, rmn) = inAndRmn
      exprAndRmn <- expression(rmn)
    } yield exprAndRmn
  }

  // x (a, b, c)
  def app[F[_]](
      tokens: List[Token]
  )(implicit me: MonadError[F, CompilerError]): F[ParserOut] = {

    for {
      bodyAndRmn <- identifier(tokens)
      leftParenAndRmn <- consume(Operator.LeftParen, bodyAndRmn.rmn)
      (_, rmnAfterLP) = leftParenAndRmn
      argAndRemaining <- appArgs(bodyAndRmn.expr, rmnAfterLP)
      rParenAndRmn <- consume(Operator.RightParen, argAndRemaining.rmn)
    } yield ParserOut(argAndRemaining.expr, rParenAndRmn._2)

  }

  def appArgs[F[_]](
      previousExpr: Expr,
      tokens: List[Token]
  )(implicit me: MonadError[F, CompilerError]): F[ParserOut] = {

    for {
      argAndRemaining <- expression(tokens)
      nextResult <- (for {
        commaAndTokens <- consume(Operator.Comma, argAndRemaining.rmn)
        (comma, afterComma) = commaAndTokens
        rs <- appArgs(App(previousExpr, argAndRemaining.expr), afterComma)
      } yield (rs)).recover({ case CompilerError.ExpectToken(Operator.Comma) =>
        ParserOut(App(previousExpr, argAndRemaining.expr), argAndRemaining.rmn)
      })

    } yield nextResult

  }

  def lambdaFunc[F[_]](
      tokens: List[Token]
  )(implicit me: MonadError[F, CompilerError]): F[ParserOut] = {

    for {
      funTokensAndRemaining <- consume(Keyword.Fun, tokens)
      (funToken, rmn) = funTokensAndRemaining
      result <- lambda(rmn)
    } yield (result)
  }

  def lambda[F[_]](
      tokens: List[Token]
  )(implicit me: MonadError[F, CompilerError]): F[ParserOut] = {

    for {
      argAndRemaining <- identifier(tokens)
      variable <- asVariable(argAndRemaining.expr)
      bodyAndRmn <- lambda(argAndRemaining.rmn).recoverWith(_ =>
        lamdaBody(argAndRemaining.rmn)
      )

    } yield ParserOut(
      Abs(variableName = variable, variableType = TAny, body = bodyAndRmn.expr),
      bodyAndRmn.rmn
    )
  }

  def lamdaBody[F[_]](
      tokens: List[Token]
  )(implicit me: MonadError[F, CompilerError]): F[ParserOut] = {
    for {
      arrowAndRmn <- consume(Operator.Arrow, tokens)
      (arrow, rmn) = arrowAndRmn
      bodyExpr <- expression(rmn)
    } yield (bodyExpr)

  }

  def or[F[_]](
      tokens: List[Token]
  )(implicit a: MonadError[F, CompilerError]): F[ParserOut] =
    binary(orOp, and)(tokens)

  def and[F[_]](
      tokens: List[Token]
  )(implicit a: MonadError[F, CompilerError]): F[ParserOut] =
    binary(andOp, equality)(tokens)

  // Parse binary expressions that share this grammar
  // ```
  //    expr   -> descendant (OPERATOR descendant)  *
  // ```
  // Consider "equality" expression as an example. Its direct descendant is "comparison"
  // and its OPERATOR is ("==" | "!=").

  def binary[F[_]](
      op: Token => F[(Expr, Expr) => Expr],
      descendantFn: List[Token] => F[ParserOut]
  )(
      tokens: List[Token]
  )(implicit a: MonadError[F, CompilerError]): F[ParserOut] =

    def matchOp(ts: List[Token], leftExpr: Expr): F[ParserOut] =
      ts match
        case token :: rest =>
          op(token)
            .flatMap(fn => {
              for {
                rightExprAndRmn <- descendantFn(rest)
                continueResult <- matchOp(
                  rightExprAndRmn.rmn,
                  fn(leftExpr, rightExprAndRmn.expr)
                )
              } yield continueResult
            })
            .orElse(a.pure(ParserOut(leftExpr, ts)))

        case _ => a.pure(ParserOut(leftExpr, ts))

    descendantFn(tokens).flatMap((parserOut) =>
      matchOp(parserOut.rmn, parserOut.expr)
    )

  def equality[F[_]](
      tokens: List[Token]
  )(implicit a: MonadError[F, CompilerError]): F[ParserOut] =
    binary(equalityOp, comparison)(tokens)

  def comparison[F[_]](
      tokens: List[Token]
  )(implicit a: MonadError[F, CompilerError]): F[ParserOut] =
    binary(comparisonOp, term)(tokens)

  def term[F[_]](
      tokens: List[Token]
  )(implicit a: MonadError[F, CompilerError]): F[ParserOut] =
    binary(termOp, factor)(tokens)

  def factor[F[_]](
      tokens: List[Token]
  )(implicit a: MonadError[F, CompilerError]): F[ParserOut] =
    binary(factorOp, unary)(tokens)

  def unary[F[_]](
      tokens: List[Token]
  )(implicit a: MonadError[F, CompilerError]): F[ParserOut] =
    tokens match {
      case token :: rest =>
        (for {
          fn <- unaryOp(token)
          result <- unary(rest)
        } yield (ParserOut(fn(result.expr), result.rmn))).recoverWith(_ =>
          primaryOrIdentiferOrGroup(tokens)
        )

      case _ =>
        primaryOrIdentiferOrGroup(tokens)
    }

  def primaryOrIdentiferOrGroup[F[_]](
      tokens: List[Token]
  )(implicit a: MonadError[F, CompilerError]): F[ParserOut] =
    app(tokens).orElse(
      primary(tokens).orElse(identifier(tokens)).orElse(group(tokens))
    )

  def primary[F[_]](
      tokens: List[Token]
  )(implicit a: MonadError[F, CompilerError]): F[ParserOut] =
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

  def identifier[F[_]](
      tokens: List[Token]
  )(implicit a: MonadError[F, CompilerError]): F[ParserOut] =
    tokens match {
      case Literal.Identifier(name) :: rest =>
        a.pure(ParserOut(Expression.Variable(Literal.Identifier(name)), rest))
      case _ => a.raiseError(CompilerError.ExpectExpression(tokens))
    }

  def asVariable[F[_]](x: Expr)(implicit
      a: MonadError[F, CompilerError]
  ): F[Variable] = {
    x.match {
      case v: Variable => a.pure(v)
      case _           => a.raiseError(CompilerError.ExpectIdentifer())
    }

  }

  def group[F[_]](
      tokens: List[Token]
  )(implicit a: MonadError[F, CompilerError]): F[ParserOut] = {
    tokens match
      case Operator.LeftParen :: rest => parenBody(rest)
      case _ => a.raiseError(CompilerError.ExpectExpression(tokens))

  }

  // Parse the body within a pair of parentheses (the part after "(")
  def parenBody[F[_]](
      tokens: List[Token]
  )(implicit a: MonadError[F, CompilerError]): F[ParserOut] =
    expression(tokens).flatMap((parserOut) =>
      parserOut.rmn match
        case Operator.RightParen :: rmn =>
          a.pure(ParserOut(Expression.Grouping(parserOut.expr), rmn))
        case _ => a.raiseError(CompilerError.ExpectClosing(parserOut.rmn))
    )

  def consume[F[_]](
      expect: Token,
      tokens: List[Token]
  )(implicit a: MonadError[F, CompilerError]): F[(Token, List[Token])] =
    tokens.headOption match {
      case Some(token) => {
        if (token == expect) {
          a.pure((expect, tokens.tail))
        } else {
          a.raiseError(CompilerError.ExpectToken(expect))
        }
      }
      case _ => a.raiseError(CompilerError.ExpectToken(expect))
    }

  case class ParserOut(expr: Expr, rmn: List[Token])

  def orOp[F[_]](token: Token)(implicit
      me: MonadError[F, CompilerError]
  ): F[(Expr, Expr) => Expr] =
    token match
      case Keyword.Or =>
        me.pure((l, r) => Buildin(BuildinFn.Logical(BuildinFn.Or, l, r)))
      case _ =>
        me.raiseError(
          CompilerError.ExpectToken(
            Keyword.Or
          )
        )

  def andOp[F[_]](token: Token)(implicit
      me: MonadError[F, CompilerError]
  ): F[(Expr, Expr) => Expr] =
    token match
      case Keyword.And =>
        me.pure((l, r) => Buildin(BuildinFn.Logical(BuildinFn.And, l, r)))
      case _ =>
        me.raiseError(
          CompilerError.ExpectToken(
            Keyword.And
          )
        )

  def equalityOp[F[_]](token: Token)(implicit
      me: MonadError[F, CompilerError]
  ): F[(Expr, Expr) => Expr] =
    token match
      case Operator.EqualEqual =>
        me.pure((l, r) => Buildin(BuildinFn.Comparison(BuildinFn.Equal, l, r)))
      case Operator.BangEqual =>
        me.pure((l, r) =>
          Buildin(BuildinFn.Comparison(BuildinFn.NotEqual, l, r))
        )
      case _ =>
        me.raiseError(
          CompilerError.ExpectTokens(
            List(
              Operator.EqualEqual,
              Operator.BangEqual
            )
          )
        )

  def comparisonOp[F[_]](token: Token)(implicit
      me: MonadError[F, CompilerError]
  ): F[(Expr, Expr) => Expr] =
    token match
      case Operator.Less =>
        me.pure((l, r) => Buildin(BuildinFn.Comparison(BuildinFn.Less, l, r)))
      case Operator.LessEqual =>
        me.pure((l, r) =>
          Buildin(BuildinFn.Comparison(BuildinFn.LessEqual, l, r))
        )
      case Operator.Greater =>
        me.pure((l, r) =>
          Buildin(BuildinFn.Comparison(BuildinFn.Greater, l, r))
        )
      case Operator.GreaterEqual =>
        me.pure((l, r) =>
          Buildin(BuildinFn.Comparison(BuildinFn.GreaterEqual, l, r))
        )
      case _ =>
        me.raiseError(
          CompilerError.ExpectTokens(
            List(
              Operator.Less,
              Operator.LessEqual,
              Operator.Greater,
              Operator.GreaterEqual
            )
          )
        )

  def termOp[F[_]](token: Token)(implicit
      me: MonadError[F, CompilerError]
  ): F[(Expr, Expr) => Expr] =
    token match
      case Operator.Plus =>
        me.pure((l, r) => Buildin(BuildinFn.Arithmetic(BuildinFn.Add, l, r)))

      case Operator.Minus =>
        me.pure((l, r) => Buildin(BuildinFn.Arithmetic(BuildinFn.Sub, l, r)))

      case _ =>
        me.raiseError(CompilerError.ExpectTokens(List(Operator.Plus, Operator.Minus)))

  def factorOp[F[_]](token: Token)(implicit
      me: MonadError[F, CompilerError]
  ): F[(Expr, Expr) => Expr] =
    token match

      case Operator.Star =>
        me.pure((l, r) => Buildin(BuildinFn.Arithmetic(BuildinFn.Mul, l, r)))
      case Operator.Slash =>
        me.pure((l, r) => Buildin(BuildinFn.Arithmetic(BuildinFn.Div, l, r)))
      case _ =>
        me.raiseError(CompilerError.ExpectTokens(List(Operator.Star, Operator.Slash)))

  def unaryOp[F[_]](token: Token)(implicit
      me: MonadError[F, CompilerError]
  ): F[Expr => Expr] =
    token match
      case x @ Operator.Minus =>
        me.pure(x => Buildin(BuildinFn.Unary(BuildinFn.Negate, x)))
      case x @ Operator.Bang =>
        me.pure(x => Buildin(BuildinFn.Unary(BuildinFn.Not, x)))
      case _ =>
        me.raiseError(CompilerError.ExpectTokens(List(Operator.Minus, Operator.Bang)))

}
