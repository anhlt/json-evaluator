package io.jseval.parser

import cats.MonadError
import io.jseval.Parser.ParserOut
import io.jseval.Token
import io.jseval.CompilerError

trait PrefixParser {
  def parse[F[_]](JSParser: JSParser, tokens: List[Token])(implicit a: MonadError[F, CompilerError]): F[ParserOut] = ???
}


case class LiteralParser() extends PrefixParser

case class ParenthesisParser() extends PrefixParser

case class BracketPrefixParser() extends PrefixParser

case class BracePrefixParser() extends PrefixParser
