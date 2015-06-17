package typecheck.inference

import typecheck.CustomMatchers
import util.UnitSpec
import util.Implicits._

import UnitPairContext._
import BooleanContext._


class CaseTest extends UnitSpec with CustomMatchers {
  it should "infer the type of if-then-else-clause" in {
    bif("tt", "ff", "tt") should haveTypeInContext(envWithBBoolean, "Bool")
  }
}
