package typecheck.inference

import terms.Variables.vv
import terms._
import typecheck.CustomMatchers
import typecheck.Environment.EnvValue
import typecheck.inference.BooleanContext._
import util.Implicits._
import util.UnitSpec

import scala.collection.Map
import scala.collection.immutable.{Map => IMap}


/**
 * Various tests for Bools
 */
class BooleanEnvTest extends UnitSpec with CustomMatchers {

//  it should "ff : Bool, tt : Bool" in {
//    fff should haveTypeInContext(extendedBoolEnv, "Bool")
//    ftt should haveTypeInContext(extendedBoolEnv, "Bool")
//  }
//
//  it should "(if tt then tt else tt) -> tt" in {
//    Var("if").app(ftt, ftt, ftt) should beBequivalentTo(extendedBoolEnv, ftt)
//  }
//
//  it should "(if tt then ff else tt) -> ff" in {
//    Var("if").app(ftt, fff, ftt) should beBequivalentTo(extendedBoolEnv, fff)
//  }
//
//  it should "isTrue tests" in {
//    /**
//     * isTrue = \a.if (a) then true else false
//     */
//    // TODO val isTrueTerm = "a".lam("if".app("a", "a", fff)) should work
//    val isTrueTerm = "a".lam("Bool", "if".app("a", "a", fff))
//    val env = extendedBoolEnv ++ Map(
//      vv("isTrue") -> EnvValue(TVar.dummy, isTrueTerm)
//    )
//
//    Var("isTrue") should haveTypeInContext(env, ".".pi("Bool", "Bool"))
//    Var("isTrue").app(ftt) should beBequivalentTo(env, ftt)
//    Var("isTrue").app(fff) should beBequivalentTo(env, fff)
//  }
//
//  it should "isTrue tests 2" in {
//    /**
//     * isTrue = \a.match (a) {tt -> tt, ff -> ff}
//     */
//    val isTrueTerm = "a".lam("Bool", "a".ccase(Map("tt" -> ftt, "ff" -> fff)))
//    val env = extendedBoolEnv ++ Map(
//      vv("isTrue") -> EnvValue(TVar.dummy, isTrueTerm)
//    )
//
//    Var("isTrue") should haveTypeInContext(env, ".".pi("Bool", "Bool"))
//    Var("isTrue").app(ftt) should beBequivalentTo(env, ftt)
//    Var("isTrue").app(fff) should beBequivalentTo(env, fff)
//  }
//
//
//  it should "first function tests" in {
//    /**
//     * first = \a.\b.a
//     */
//    val firstTerm = "a".lam("b".lam("a"))
//    val first3 = "a".lam("b".lam("c".lam("first".app("first".app("a", "b"), "c"))))
//
//    val env = extendedBoolEnv ++ Map(
//      vv("first") -> EnvValue(TVar.dummy, firstTerm),
//      vv("first3") -> EnvValue(TVar.dummy, first3)
//    )
//
//    Var("first").app(ftt, fff) should beBequivalentTo(env, ftt)
//    Var("first").app(ftt, ftt) should beBequivalentTo(env, ftt)
//    Var("first").app(fff, fff) should beBequivalentTo(env, fff)
//    Var("first").app(fff, ftt) should beBequivalentTo(env, fff)
//    Var("first").app("first".app(ftt, ftt), ftt) should beBequivalentTo(env, ftt)
//    Var("first").app("first".app(ftt, fff), ftt) should beBequivalentTo(env, ftt)
//    Var("first3").app(ftt, ftt, ftt) should beBequivalentTo(env, ftt)
//  }
//
//  it should "and function tests" in {
//    Var("and").app(ftt, ftt) should beBequivalentTo(extendedBoolEnv, ftt)
//    Var("and").app(ftt, fff) should beBequivalentTo(extendedBoolEnv, fff)
//    Var("and").app(fff, ftt) should beBequivalentTo(extendedBoolEnv, fff)
//    Var("and").app(fff, fff) should beBequivalentTo(extendedBoolEnv, fff)
//  }
//
//  it should "not function tests" in {
//    Var("not").app(ftt) should beBequivalentTo(extendedBoolEnv, fff)
//    Var("not").app(fff) should beBequivalentTo(extendedBoolEnv, ftt)
//  }
//
//  it should "or function tests" in {
//    Var("or").app(ftt, ftt) should beBequivalentTo(extendedBoolEnv, ftt)
//    Var("or").app(ftt, fff) should beBequivalentTo(extendedBoolEnv, ftt)
//    Var("or").app(fff, ftt) should beBequivalentTo(extendedBoolEnv, ftt)
//    Var("or").app(fff, fff) should beBequivalentTo(extendedBoolEnv, fff)
//  }
//
//  it should "tt eqb tt, ff eqb ff are provable" in {
//    FElem("top") should haveTypeInContext(extendedBoolEnv, "eqb".app(ftt, ftt))
//    FElem("top") should haveTypeInContext(extendedBoolEnv, "eqb".app(fff, fff))
//  }
//
//  it should "infer Bool : Set" in {
//    Var("Bool") should haveTypeInContext(extendedBoolEnv, Level(0))
//  }
//
//  it should "infer if : Bool -> Bool -> Bool -> Bool type" in {
//    Var("if") should haveTypeInContext(extendedBoolEnv, Level(0)) // todo
//  }

  it should "infer not type" in {
    Var("not") should haveTypeInContext(extendedBoolEnv, ".".pi("Bool", "Bool"))
  }
//
//  it should "infer eqb type" in {
//    Var("eqb") should haveTypeInContext(extendedBoolEnv, "x".pi("Bool", "y".pi("Bool", Level(0))))
//  }
//
//  it should "rwerewr" in {
//    /**
//     * th1 : (a : Bool) → (qq : (ww : a eqb ff) → ⊥) → a eqb tt
//     */
//    val statement = "a".pi("Bool", "qq".pi("ww".pi("eqb".app("a", fff), "Bot"), "eqb".app("a", ftt)))
//    statement should haveTypeInContext(extendedBoolEnv, Level(1))
//  }
}
