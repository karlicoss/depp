package typecheck.inference

import terms.Terms.{App, Level, Var}
import terms.Variables.vv
import typecheck.CustomMatchers
import util.Terms.{makeNumeral, natContext, booleanContext}
import util.UnitSpec

/**
 * Created by karlicos on 30.05.15.
 */
class InferenceTest extends UnitSpec with CustomMatchers {

  it should "fewfwef" in {
    Var(vv("x")) should beBequivalentTo(
      Map(vv("x") -> Level(0)), Var(vv("x")))
  }

  it should "infer false :: Bool" in {
    Var(vv("false")) should haveTypeInContext(booleanContext, Var(vv("Bool")))
  }

  it should "infer succ zero :: Nat" in {
    App(Var(vv("succ")), Var(vv("zero"))) should haveTypeInContext(natContext, Var(vv("Nat")))
  }

  it should "infer 5 :: Nat" in {
    makeNumeral(5) should haveTypeInContext(natContext, Var(vv("Nat")))
  }
}
