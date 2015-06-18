package typecheck.inference

import terms.{Var, Level, Finite}
import terms.Variables.Simple
import terms.Variables.vv
import typecheck.CustomMatchers
import typecheck.Environment._
import util.Implicits._
import util.UnitSpec

import scala.collection.Map
import scala.collection.immutable.{Map => IMap}


class ImplicitTypeTest extends UnitSpec with CustomMatchers {

  val simpleContext2: Environment = IMap(
    vv("A") -> Level(0),
    vv("a") -> Var("A")
  )

  it should "infer implicit type based on argument type" in {
    ("x".lam("x")).app("a").undummy() should haveTypeInContext(simpleContext2, "A")
  }
}
