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

  def identChar = lexical.letter

  lexical.delimiters ++= Seq(
    "\\", "λ", // lambda
    "->", "=>", // pi
    ".", // lambda
    ",", // dependent pair constructor, finite type
    "(", ")", // dependent pair constructor
    "{", "}", // finite
    "#", // level
    ":", // type annotation
    "@" // finite element identifier
  )
  lexical.reserved ++= Seq(
    "forall",
    "exists",
    "case",
    "Type"
  )

  lazy val expr: Parser[Term] = term

  lazy val term: Parser[Term] = appterm

  lazy val aterm: Parser[Term] =
      varname ^^ (Var(_)) |
      felem |
      finite |
      pair |
      level |
      lam | pi | sigma |
      "(" ~> expr <~ ")"

  lazy val appterm: Parser[Term] =
      rep1(aterm) ^^ (x => if (x.length == 1) x.head else x.head.app(x.tail: _*))

  lazy val varname: Parser[Variable] = ident ^^ (Simple(_))

  lazy val felem: Parser[FElem] = "@" ~> ident ^^ (FElem(_))

  lazy val level: Parser[Level] =
    "Type" ~> "#" ~> numericLit ^^ (s => Level(s.toInt)) |
    "Type" ^^ (_ => Level(0))

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

  def delimParser[T](delim: Parser[String], p: Parser[T]): Parser[List[T]] = {
    rep(p <~ delim) ~ opt(p) ^^ flatten2((a, b) => b match {
      case Some(x) => a :+ x
      case None => a
    })
  }

  lazy val finite: Parser[Finite] =
    "{" ~> delimParser(",", ident) <~ "}" ^^ (x => Finite(x.toSet))


  def parse(source: String): ParseResult[Any] = {
    val tokens = new lexical.Scanner(source)
    phrase(expr)(tokens)
  }
}
