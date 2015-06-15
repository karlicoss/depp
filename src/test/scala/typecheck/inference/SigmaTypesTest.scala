package typecheck.inference

import terms.Abstraction.Abs
import terms.Variables.vv
import terms.{Term, Finite, Level, Sigma}
import typecheck.CustomMatchers
import typecheck.Environment.EnvValue
import util.Implicits._
import util.UnitSpec

class SigmaTypesTest extends UnitSpec with CustomMatchers {

  val Unit = Finite(List("unit"))

  def pair(a: Term, b: Term): Term = Sigma(Abs(".", a, b))

  val envWithUnit = Map(vv("Unit") -> EnvValue(Level(0), Unit))
  
  it should "infer type of simple sigma type" in {
    pair("Unit", "Unit") should haveTypeInContext(envWithUnit, Level(0))
  }
  
  it should "infer type of sinmple sigme type 2" in {
    pair(pair("Unit", "Unit"), pair("Unit", "Unit")) should haveTypeInContext(envWithUnit, Level(0))
  }
}
