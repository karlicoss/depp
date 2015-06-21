package typecheck.inference

import terms._
import typecheck.CustomMatchers
import util.UnitSpec
import util.Implicits._
import UnitPairContext._

class DPairInferenceTest extends UnitSpec with CustomMatchers {

  val fuu = FElem("uu")

  it should "infer dependend pair type" in {
    DPair(fuu, fuu, null)
  }

  it should "evaluate simple proj1" in {
    Proj1(DPair(fuu, fuu)) should beBequivalentTo(envWithUnit, fuu)
  }

  it should "evaluate simple proj2" in {
    Proj2(DPair(fuu, fuu)) should beBequivalentTo(envWithUnit, fuu)
  }

  it should "infer proj1 type [simple]" in {
    Proj1(DPair(fuu, fuu)) should haveTypeInContext(envWithUnit, "Unit")
  }

  it should "infer proj2 type [simple]" in {
    Proj2(DPair(fuu, fuu)) should haveTypeInContext(envWithUnit, "Unit")
  }

  it should "infer proj2 type [levels]" in {
    Proj2(DPair(Level(0), Level(0))) should haveTypeInContext(Map(), Level(1))
  }

  it should "infer proj2 type [simple][2]" in {
    Proj2(DPair("Unit", "Unit")) should haveTypeInContext(envWithUnit, Level(0))
  }
}
