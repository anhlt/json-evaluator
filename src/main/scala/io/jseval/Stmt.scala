package io.jseval
import Expression._
import Literal._

enum Stmt:
  case Var(name: Option[Identifier], expr: Expression.Expr)
