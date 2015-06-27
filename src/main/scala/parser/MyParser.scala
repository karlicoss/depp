package parser

import terms.Variables.{Simple, Variable}
import terms._
import typecheck.Environment.{EnvValue, Environment}

import scala.collection.immutable.{Map => IMap}
import scala.util.parsing.combinator.ImplicitConversions
import scala.util.parsing.combinator.lexical.StdLexical
import scala.util.parsing.combinator.syntactical.StdTokenParsers

class MyParser() extends StdTokenParsers
  with ImplicitConversions {
  override type Tokens = StdLexical
  override val lexical = new Tokens

  def identChar = lexical.letter

  lexical.delimiters ++= Seq(
    "\\", // lambda
    "->", // function
    ".", // abstraction separator
    ",", // dependent pair constructor, finite type
    "(", ")", // dependent pair constructor
    "{", "}", // finite
    "#", // level
    ":", // type annotation
    "=>", // pattern separator
    ";", // cases separator, definitions separator
    "@", // finite element identifier
    "=" // definition
  )

  lexical.reserved ++= Seq(
    "forall", "∀", // forall
    "exists", "∃", // exists
    "fun", "λ", // lambda
    "elim", "default",
    "Type",
    "break", "with", "in" // break (pair) with (f, s) in { }
  )

  lazy val lambda: Parser[String] =
    "fun" | "\\" | "λ"

  lazy val forall: Parser[String] =
    "forall" | "∀"

  lazy val exists: Parser[String] =
    "exists" | "∃"

  lazy val program: Parser[(Environment, Term)] =
    rep(dfn <~ ";") ~ expr ^^ flatten2((dfns, prog) => (dfns.toMap, prog))

  lazy val expr: Parser[Term] = term

  lazy val term: Parser[Term] = appterm

  lazy val aterm: Parser[Term] =
      varname ^^ (Var(_)) |
      felem |
      finite |
      pair | brk |
      level |
      ccase |
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

  def absParser(s: Parser[String]): Parser[(Variable, Option[Term], Term)] = s ~> abs

  lazy val pair: Parser[DPair] =
    ("(" ~> term <~ ",") ~ (term <~ ")") ^^ flatten2((fst, snd) => DPair(fst, snd))

  lazy val lam: Parser[Lam] =
    absParser(lambda) ^^ (x => x._2 match {
      case Some(tp) => x._1.lam(tp, x._3)
      case None => x._1.lam(x._3)
    })

  lazy val pi: Parser[Pi] =
    absParser(forall) ^^ (x => x._2 match {
      case Some(tp) => x._1.pi(tp, x._3)
      case None => throw ParserException("TODO")
    })

  lazy val sigma: Parser[Sigma] =
    absParser(exists) ^^ (x => x._2 match {
      case Some(tp) => Sigma.create(x._1, tp, x._3)
      case None => throw ParserException("TODO")
    })

  lazy val finite: Parser[Finite] =
    "{" ~> repsep(ident, ",") <~ "}" ^^ (x => Finite(x.toSet))

  // TODO type?
  lazy val ccase: Parser[Case] =
    ("elim" ~> "(" ~> expr <~ ")") ~ ("{" ~> cases) ~ (opt(dflt) <~ "}") ^^ flatten3((a, b, c) => {
      c match {
        case Some(df) => a.ccase(b.toMap, df)
        case None => a.ccase(b.toMap)
      }
    })

  lazy val cases: Parser[List[(FElem.FElemType, Term)]] =
    rep(ident ~ ("=>" ~> expr <~ ";") ^^ (x => (x._1, x._2)))

  lazy val dflt: Parser[Term] =
    ("default" ~ "=>") ~> expr <~ ";"

  lazy val dfn: Parser[(Variable, EnvValue)] =
    varname ~ opt(":" ~> expr) ~ ("=" ~> expr) ^^ flatten3((a, b, c) => {
      b match {
        case Some(x) => (a, EnvValue(x, c))
        case None => (a, EnvValue.auto(c))
      }
    })

  lazy val brk: Parser[Break] =
    ("break" ~> expr <~ "with") ~
      ("(" ~> varname <~ ",") ~
      (varname <~ ")") ~
      ("in" ~> expr) ^^ flatten4((what, f, s, body) => Break(what, f, s, body))

  def parse(source: String): ParseResult[(Environment, Term)] = {
    val tokens = new lexical.Scanner(source)
    phrase(program)(tokens)
  }
}

object MyParser {
  def apply(): MyParser = new MyParser
}