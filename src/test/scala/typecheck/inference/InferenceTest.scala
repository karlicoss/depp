package typecheck.inference

import terms.Terms.{Level, Var}
import terms.Variables.vv
import typecheck.CustomMatchers
import typecheck.Environment.Environment
import util.Implicits._
import util.Terms.{booleanContext, makeNumeral, natContext, churchBooleanContext}
import util.UnitSpec

/**
 * Created by karlicos on 30.05.15.
 */
class InferenceTest extends UnitSpec with CustomMatchers {

  it should "fewfwef" in {
    Var(vv("x")) should beBequivalentTo(
      Map(vv("x") -> Level(0)), "x")
  }

  it should "infer false :: Bool" in {
    Var("false") should haveTypeInContext(booleanContext, "Bool")
  }

  it should "infer succ zero :: Nat" in {
    "succ".app("zero") should haveTypeInContext(natContext, "Nat")
  }

  it should "infer 5 :: Nat" in {
    makeNumeral(5) should haveTypeInContext(natContext, "Nat")
  }

  val simpleContext: Environment = Map(
    vv("X") -> Level(0)
  )

  it should "infer type of identity function" in {
    "x".lam("X", "x") should haveTypeInContext(simpleContext, ".".pi("X", "X")) // TODO alpha conversion
  }

  it should "fafsdf" in {
    "not".app("tt") should haveTypeInContext(churchBooleanContext, "Boolean")
  }
}
