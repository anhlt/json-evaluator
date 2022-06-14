package io.jseval

import cats._
import cats.implicits._
import Expression._
import cats.effect.kernel.syntax.resource
import io.jseval.Literal
import cats.instances._

object Parser {

  enum Error(msg: String, tokens: List[Token]):
    case ExpectExpression(tokens: List[Token])
        extends Error("ExpectExpression", tokens)
    case ExpectClosing(tokens: List[Token])
        extends Error("Expect ')' after expression", tokens)
    case InvalidAssignmentTartget(token: Token)
        extends Error("Invalid assignment target.", List(token))

  def parseExpression[F[_]](tokens: List[Token])(implicit
      ae: ApplicativeError[F, Error]
  ): F[Expr[Any]] = ???

  def parse(ts: List[Token]): ExprParser[LiteralType] = expression(ts)

  type ExprParser[T] = Either[Error, (Expr[T], List[Token])]

  type BinaryOp[T] = Token => Option[(Expr[T], Expr[T]) => Expr[T]]

  type UnaryOp[T] = Token => Option[Expr[T] => Expr[T]]

  def expression: List[Token] => ExprParser[LiteralType] = or

  def or: List[Token] => ExprParser[LiteralType] = binary(orOp, and)

  def and: List[Token] => ExprParser[LiteralType] = binary(andOp, equality)

  // Parse binary expressions that share this grammar
  // ```
  //    expr   -> descendant (OPERATOR descendant)  *
  // ```
  // Consider "equality" expression as an example. Its direct descendant is "comparison"
  // and its OPERATOR is ("==" | "!=").
  def binary[T](
      op: BinaryOp[T],
      descendant: List[Token] => ExprParser[T]
  )(
      tokens: List[Token]
  ): ExprParser[T] =

    def matchOp(ts: List[Token], l: Expr[T]): ExprParser[T] =
      ts match
        case token :: rest =>
          op(token) match
            case Some(fn) =>
              descendant(rest).flatMap((r: Expr[T], rmn: List[Token]) =>
                matchOp(rmn, fn(l, r))
              )
            case None => Right(l, ts)
        case _ => Right(l, ts)

    descendant(tokens).flatMap((expr, rest) => matchOp(rest, expr))

  def equality: List[Token] => ExprParser[LiteralType] =
    binary(equalityOp, comparison)
  def comparison: List[Token] => ExprParser[LiteralType] =
    binary(comparisonOp, term)
  def term: List[Token] => ExprParser[LiteralType] = binary(termOp, factor)
  def factor: List[Token] => ExprParser[LiteralType] =
    binary(factorOp, unary)

  def unary(tokens: List[Token]): ExprParser[LiteralType] =
    tokens match
      case token :: rest =>
        unaryOp(token) match
          case Some(fn) =>
            unary(rest).flatMap((expr, rmn) => Right(fn(expr), rmn))
          case None => primary(tokens)
      case _ => primary(tokens)

  def primary(tokens: List[Token]): ExprParser[LiteralType] =
    tokens match
      case Literal.Number(l) :: rest =>
        Right(Expression.Literal(l.toDouble), rest)
      case Literal.Str(l) :: rest => Right(Expression.Literal(l), rest)
      case Keyword.True :: rest   => Right(Expression.Literal(true), rest)
      case Keyword.False :: rest  => Right(Expression.Literal(false), rest)
      case Keyword.Null :: rest   => Right(Expression.Literal(null), rest)
      case Literal.Identifier(name) :: rest =>
        Right(Expression.Variable(Literal.Identifier(name)), rest)
      case Operator.LeftParen :: rest => parenBody(rest)
      case _                          => Left(Error.ExpectExpression(tokens))

  // Parse the body within a pair of parentheses (the part after "(")
  def parenBody(
      tokens: List[Token]
  ): ExprParser[LiteralType] = expression(tokens).flatMap((expr, rest) =>
    rest match
      case Operator.RightParen :: rmn => Right(Expression.Grouping(expr), rmn)
      case _                          => Left(Error.ExpectClosing(rest))
  )

  val orOp: BinaryOp[LiteralType] =
    case Keyword.Or => Some(Expression.Or.apply)
    case _          => None

  val andOp: BinaryOp[LiteralType] =
    case Keyword.And => Some(Expression.And.apply)
    case _           => None

  val equalityOp: BinaryOp[LiteralType] =
    case Operator.EqualEqual => Some(Expression.Equal.apply)
    case Operator.BangEqual  => Some(Expression.NotEqual.apply)
    case _                   => None

  val comparisonOp: BinaryOp[LiteralType] =
    case Operator.Less         => Some(Expression.Less.apply)
    case Operator.LessEqual    => Some(Expression.LessEqual.apply)
    case Operator.Greater      => Some(Expression.Greater.apply)
    case Operator.GreaterEqual => Some(Expression.GreaterEqual.apply)
    case _                     => None

  val termOp: BinaryOp[LiteralType] =
    case Operator.Plus  => Some(Expression.Add.apply)
    case Operator.Minus => Some(Expression.Subtract.apply)
    case _              => None

  val factorOp: BinaryOp[LiteralType] =
    case Operator.Star  => Some(Expression.Multiply.apply)
    case Operator.Slash => Some(Expression.Divide.apply)
    case _              => None

  val unaryOp: UnaryOp[LiteralType] =
    case Operator.Minus => Some(Expression.Negate.apply)
    case Operator.Bang  => Some(Expression.Not.apply)
    case _              => None

}
