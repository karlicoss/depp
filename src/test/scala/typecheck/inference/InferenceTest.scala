package typecheck.inference

import terms.Terms.{Level, Var}
import terms.Variables.vv
import typecheck.CustomMatchers
import util.Terms.booleanContext
import util.UnitSpec

/**
 * Created by karlicos on 30.05.15.
 */
class InferenceTest extends UnitSpec with CustomMatchers {

  it should "fewfwef" in {
    Var(vv("x")) should beBequivalentTo(
      Map(vv("x") -> Level(0)), Var(vv("x")))
  }

  it should "infer type" in {
    Var(vv("false")) should haveTypeInContext(booleanContext, Var(vv("Bool")))
  }


}
