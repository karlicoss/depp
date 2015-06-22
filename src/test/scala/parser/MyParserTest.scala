package parser

import org.scalatest.{FlatSpec, ShouldMatchers}
import terms.{DPair, App, Lam}
import terms.Variables.Simple
import typecheck.CustomMatchers
import util.UnitSpec

class MyParserTest extends UnitSpec {

  object p extends MyParser {
    def parsing[T](s: String)(implicit p: Parser[T]): T = {
      //wrap the parser in the phrase parse to make sure all input is consumed
      val phraseParser = phrase(p)
      //we need to wrap the string in a reader so our parser can digest it
      val input = new lexical.Scanner(s)
      phraseParser(input) match {
        case Success(t,_)     => t
        case NoSuccess(msg,_) => throw new IllegalArgumentException(
          "Could not parse '" + s + "': " + msg)
      }
    }

    def assertFail[T](input:String)(implicit p:Parser[T]) {
      evaluating(parsing(input)(p)) should produce[IllegalArgumentException]
    }

  }
  
  it should "parse variable" in {
    implicit val parserToTest = p.varname
    p.parsing("alala") should equal(Simple("alala"))
    p.parsing("fwefwef")
  }

  it should "parse finite element" in {
    implicit val ptest = p.felem
    p.parsing("@fsdfs")
  }

  it should "parse app" in {
    implicit val ptest = p.appterm
    p.parsing("x x x") shouldBe an[App]
    p.parsing("x (y z)") shouldBe an[App]
  }

  it should "parse lambda" in {
    implicit val ptest = p.lam
    p.parsing("\\x.x") shouldBe an[Lam]
    p.parsing("\\a:T.a") shouldBe an[Lam]
    p.parsing("\\a:T.a a") shouldBe an[Lam]
  }

  it should "parse dependent pair" in {
    implicit val ptest = p.pair
    p.parsing("(a, b)") shouldBe an[DPair]
  }

  it should "other tests" in {
    implicit val ptest = p.expr
    p.parsing("(x)")
    p.parsing("((x))")
    p.parsing("((x, y))")
  }

}
