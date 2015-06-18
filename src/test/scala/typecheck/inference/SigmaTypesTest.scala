package typecheck.inference

import terms._
import typecheck.CustomMatchers
import typecheck.inference.UnitPairContext._
import util.Implicits._
import util.UnitSpec

class SigmaTypesTest extends UnitSpec with CustomMatchers {

  it should "infer type of simple sigma" in {
    PairType("Unit", "Unit") should haveTypeInContext(envWithUnit, Level(0))
  }

  it should "infer type of simple sigma [2]" in {
    PairType(PairType("Unit", "Unit"), PairType("Unit", "Unit")) should haveTypeInContext(envWithUnit, Level(0))
  }

  it should "infer type of dependent pair" in {
    DPair("uu", "uu") should haveTypeInContext(envWithUnit, PairType("Unit", "Unit"))
  }
}
