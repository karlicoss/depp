package typecheck.inference

import terms.Abstraction.Abs
import terms.Variables.vv
import terms._
import typecheck.CustomMatchers
import typecheck.Environment.EnvValue
import util.Implicits._
import util.UnitSpec

class SigmaTypesTest extends UnitSpec with CustomMatchers {

  val Unit = Finite(List("unit"))

  def PairType(a: Term, b: Term): Term = Sigma(Abs(".", a, b))

  val envWithUnit = Map(vv("Unit") -> EnvValue(Level(0), Unit))
  
  it should "infer type of simple sigma type" in {
    PairType("Unit", "Unit") should haveTypeInContext(envWithUnit, Level(0))
  }
  
  it should "infer type of simple sigma type 2" in {
    PairType(PairType("Unit", "Unit"), PairType("Unit", "Unit")) should haveTypeInContext(envWithUnit, Level(0))
  }

  it should "infer Sigma types" in {
    DPair("unit", "unit") should haveTypeInContext(envWithUnit, PairType("Unit", "Unit"))
  }
}
