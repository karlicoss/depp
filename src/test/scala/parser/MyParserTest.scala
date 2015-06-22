package parser

import org.scalatest.{FlatSpec, ShouldMatchers}
import terms._
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
      an[IllegalArgumentException] shouldBe thrownBy(parsing(input)(p))
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

  it should "parse finite types" in {
    implicit val ptest = p.finite
    p.parsing("{}") shouldBe an[Finite]
    p.parsing("{a}") shouldBe an[Finite]
    p.parsing("{a, b}") shouldBe an[Finite]
    p.parsing("{a, b, d}") shouldBe an[Finite]
    p.failure("{a, @b, d}")
  }

  it should "parse level" in {
    implicit val ptest = p.level
    p.parsing("Type") shouldEqual Level(0)
    p.parsing("Type#2") shouldEqual Level(2)
    p.parsing("Type#32") shouldEqual Level(32)
  }

  it should "parse proj1" in {
    implicit val ptest = p.fst
    p.parsing("fst pair") shouldBe an[Proj1]
  }

  it should "parse proj2" in {
    implicit val ptest = p.snd
    p.parsing("snd pair") shouldBe an[Proj2]
  }

  it should "other tests" in {
    implicit val ptest = p.expr
    p.parsing("(x)")
    p.parsing("((x))")
    p.parsing("((x, y))")
  }

}
