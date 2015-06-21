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

  it should "ff : Bool, tt : Bool" in {
    fff should haveTypeInContext(extendedBoolEnv, "Bool")
    ftt should haveTypeInContext(extendedBoolEnv, "Bool")
  }

  it should "(if tt then tt else tt) -> tt" in {
    Var("if").app(ftt, ftt, ftt) should beBequivalentTo(extendedBoolEnv, ftt)
  }

  it should "(if tt then ff else tt) -> ff" in {
    Var("if").app(ftt, fff, ftt) should beBequivalentTo(extendedBoolEnv, fff)
  }

  it should "isTrue tests" in {
    /**
     * isTrue = \a.if (a) then true else false
     */
    val isTrueTerm = "a".lam("if".app("a", "a", fff))
    val env = extendedBoolEnv ++ Map(
      vv("isTrue") -> EnvValue(TVar.dummy, isTrueTerm)
    )

    Var("isTrue").app(ftt) should beBequivalentTo(env, ftt)
    Var("isTrue").app(fff) should beBequivalentTo(env, fff)
  }

  it should "first function tests" in {
    /**
     * first = \a.\b.a
     */
    val firstTerm = "a".lam("b".lam("a"))
    val first3 = "a".lam("b".lam("c".lam("first".app("first".app("a", "b"), "c"))))

    val env = extendedBoolEnv ++ Map(
      vv("first") -> EnvValue(TVar.dummy, firstTerm),
      vv("first3") -> EnvValue(TVar.dummy, first3)
    )

    Var("first").app(ftt, fff) should beBequivalentTo(env, ftt)
    Var("first").app(ftt, ftt) should beBequivalentTo(env, ftt)
    Var("first").app(fff, fff) should beBequivalentTo(env, fff)
    Var("first").app(fff, ftt) should beBequivalentTo(env, fff)
    Var("first").app("first".app(ftt, ftt), ftt) should beBequivalentTo(env, ftt)
    Var("first").app("first".app(ftt, fff), ftt) should beBequivalentTo(env, ftt)
    Var("first3").app(ftt, ftt, ftt) should beBequivalentTo(env, ftt)
  }

  it should "and function tests" in {
    Var("and").app(ftt, ftt) should beBequivalentTo(extendedBoolEnv, ftt)
//    Var("and").app(ftt, fff) should beBequivalentTo(extendedBoolEnv, fff)
//    Var("and").app(fff, ftt) should beBequivalentTo(extendedBoolEnv, fff)
//    Var("and").app(fff, fff) should beBequivalentTo(extendedBoolEnv, fff)
  }
}
