package typecheck.inference

import terms.{Level, Proj2, Proj1, DPair}
import typecheck.CustomMatchers
import util.UnitSpec
import util.Implicits._
import UnitPairContext._

class DPairInferenceTest extends UnitSpec with CustomMatchers {

  it should "infer dependend pair type" in {
    DPair("uu", "uu", null)
  }

  it should "evaluate simple proj1" in {
    Proj1(DPair("uu", "uu")) should beBequivalentTo(envWithUnit, "uu")
  }

  it should "evaluate simple proj2" in {
    Proj2(DPair("uu", "uu")) should beBequivalentTo(envWithUnit, "uu")
  }

  it should "infer proj1 type [simple]" in {
    Proj1(DPair("uu", "uu")) should haveTypeInContext(envWithUnit, "Unit")
  }

  it should "infer proj2 type [simple]" in {
    Proj2(DPair("uu", "uu")) should haveTypeInContext(envWithUnit, "Unit")
  }

  it should "infer proj2 type [levels]" in {
    Proj2(DPair(Level(0), Level(0))) should haveTypeInContext(Map(), Level(1))
  }

  it should "infer proj2 type [simple][2]" in {
    Proj2(DPair("Unit", "Unit")) should haveTypeInContext(envWithUnit, Level(0))
  }
}
