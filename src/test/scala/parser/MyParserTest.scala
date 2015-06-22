package parser

import org.scalatest.{FlatSpec, ShouldMatchers}
import terms.Variables.Simple
import typecheck.CustomMatchers
import util.UnitSpec

class MyParserTest extends UnitSpec {

  object parser extends MyParser {
    def parsing[T](s:String)(implicit p:Parser[T]):T = {
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
    implicit val parserToTest = parser.varname
    parser.parsing("alala") should equal(Simple("alala"))
  }

}
