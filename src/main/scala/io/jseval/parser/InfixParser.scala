package io.jseval.parser

import cats.MonadError
import io.jseval.{CompilerError, Token}
import io.jseval.Expression.BuildinModule.BuildinFn
import io.jseval.Expression.{Buildin, Expr}
import io.jseval.Parser.ParserOut

trait InfixParser {
  val parser: (Expr, Expr) => Expr
  val precedence: Precendence

//  def parse[F[_]](ts: List[Token], leftExpr: Expr)(implicit a: MonadError[F, Error]): F[ParserOut] = {
//    ts match
//      case token :: rest => 
//        Grammar.
//  }
}

case object OrInfixParser extends InfixParser:
  val parser: (Expr, Expr) => Expr =
    (l, r) => Buildin(BuildinFn.Logical(BuildinFn.Or, l, r))

  val precedence: Precendence = Precendence.LOGICAL_OR

case object AndInfixParser extends InfixParser:
  val parser: (Expr, Expr) => Expr =
    (l, r) => Buildin(BuildinFn.Logical(BuildinFn.And, l, r))
  val precedence: Precendence = Precendence.LOGICAL_AND


