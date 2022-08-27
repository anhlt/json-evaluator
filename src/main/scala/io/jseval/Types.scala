package io.jseval

import Expression._
import Expression.BuildinModule._
import Expression.BuildinModule.BuildinFn._
import Expression.ValueModule._

object TypModule {
  sealed trait Typ

  case object TInt extends Typ
  case object TString extends Typ
  case object TBoolean extends Typ
  case class TArrow(argType: Typ, bodyType: Typ) extends Typ

}

