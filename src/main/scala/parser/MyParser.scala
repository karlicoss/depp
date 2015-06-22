package parser

import terms.Variables.{Simple, Variable}
import terms._

import scala.util.parsing.combinator.lexical.StdLexical
import scala.util.parsing.combinator.syntactical.StdTokenParsers
import scala.util.parsing.combinator.ImplicitConversions

class MyParser extends StdTokenParsers
  with ImplicitConversions {
  override type Tokens = StdLexical
  override val lexical = new Tokens
  lexical.delimiters ++= Seq(
    "\\", "λ",
    "->", "=>",
    ".",
    "(", ")",
    ":",
    "@"
  )
  lexical.reserved ++= Seq(
    "forall",
    "exists",
    "case"
  )

  lazy val expr: Parser[Term] = term

  lazy val term: Parser[Term] = appterm

  lazy val aterm: Parser[Term] =
      varname ^^ (Var(_)) |
      felem |
      lam | pi | sigma |
      "(" ~> expr <~ ")"

  lazy val appterm: Parser[Term] =
      rep1(aterm) ^^ (x => x.head.app(x.tail: _*))

  lazy val varname: Parser[Variable] = ident ^^ (Simple(_))

  lazy val felem: Parser[FElem] = "@" ~> ident ^^ (FElem(_))

  lazy val abs: Parser[(Variable, Option[Term], Term)] =
      varname ~ (":" ~> term) ~ ("." ~> term) ^^ flatten3((v, tp, body) => (v, Some(tp), body)) |
      varname ~ ("." ~> term) ^^ flatten2((v, body) => (v, None, body))

  def absParser(s: String): Parser[(Variable, Option[Term], Term)] = s ~> abs

  lazy val pair: Parser[DPair] =
    ("(" ~> term <~ ",") ~ (term <~ ")") ^^ flatten2((fst, snd) => DPair(fst, snd))

  lazy val lam: Parser[Lam] =
    absParser("\\") ^^ (x => x._2 match {
      case Some(tp) => x._1.lam(tp, x._3)
      case None => x._1.lam(x._3)
    })

  lazy val pi: Parser[Pi] =
    absParser("forall") ^^ (x => x._2 match {
      case Some(tp) => x._1.pi(tp, x._3)
      case None => throw ParserException("TODO")
    })

  lazy val sigma: Parser[Sigma] =
    absParser("sigma") ^^ (x => x._2 match {
      case Some(tp) => Sigma.create(x._1, tp, x._3)
      case None => throw ParserException("TODO")
    })


  def parse(source: String): ParseResult[Any] = {
    val tokens = new lexical.Scanner(source)
    phrase(expr)(tokens)
  }
}
