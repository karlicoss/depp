package parser

import scala.util.parsing.combinator.lexical.StdLexical
import scala.util.parsing.combinator.syntactical.StdTokenParsers

class Parser extends StdTokenParsers {
  override type Tokens = StdLexical
  override val lexical = new Tokens
  lexical.delimiters ++= Seq(
    "\\", "λ",
    "->", "=>",
    "forall", // TODO 
    "exists", // TODO
    ".",
    "(", ")")

  def parse(source: String): ParseResult[String] = {
    val tokens = new lexical.Scanner(source)
    phrase(ident)(tokens)
  }
}
