package typecheck.inference

import terms.Terms.{Level, Var}
import terms.Variables.vv
import typecheck.CustomMatchers
import typecheck.Environment.Environment
import util.Implicits._
import util.Terms._
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
    "x".lam("X", "x") should haveTypeInContext(simpleContext, ".".pi("X", "X"))
  }

  it should "infer type of type of identity" in {
    ".".pi("X", "X") should haveTypeInContext(simpleContext, Level(0))
  }

  it should "infer type of type of polymorphic identity function" in {
    "X".pi(Level(0), "x".pi("X", "X")) should haveTypeInContext(Map(), Level(1))
  }

  /*
    TODO: this context actually contains definitions, that is the reason for failing tests
  */
  val alalaContext: Environment = Map(
    vv("Type") -> "A".pi(Level(0), "x".pi("A", "A")),
    vv("term") -> "A".lam(Level(0), "x".lam("A", "x"))
  )

  it should "infer simple dependent types" in {
    Var("term") should haveTypeInContext(alalaContext, "Type")
  }

  it should "infer type of Church booleans" in {
    ChurchBoolean.tt should haveTypeInContext(churchBooleanContext, "Boolean")
//    ChurchBoolean.ff should haveTypeInContext(churchBooleanContext, "Boolean")
  }

//  it should "fafsdf" in {
//    "not".app("tt") should haveTypeInContext(churchBooleanContext, "Boolean")
//  }
}
