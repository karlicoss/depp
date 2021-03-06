package typecheck.inference

import terms.Variables.vv
import terms._
import typecheck.CustomMatchers
import typecheck.Environment.EnvValue.auto
import typecheck.Environment.{EnvValue, Environment}
import typecheck.inference.BooleanContext._
import typecheck.inference.MaybeBoolContext._
import typecheck.inference.UnitPairContext._
import util.Implicits._
import util.UnitSpec

class DPairInferenceTest extends UnitSpec with CustomMatchers {

  val fff = FElem("ff")
  val ftt = FElem("tt")
  val fuu = FElem("uu")

  it should "infer dependend pair type" in {
    DPair(fuu, fuu, null)
  }

  // TODO uncomment when Proj1/Proj2 are defined via Break
//  it should "evaluate simple proj1" in {
//    Proj1(DPair(fuu, fuu)) should beBequivalentTo(envWithUnit, fuu)
//  }
//
//  it should "evaluate simple proj2" in {
//    Proj2(DPair(fuu, fuu)) should beBequivalentTo(envWithUnit, fuu)
//  }
//
//  it should "infer proj1 type [simple]" in {
//    Proj1(DPair(fuu, fuu)) should haveTypeInContext(envWithUnit, "Unit")
//  }
//
//  it should "infer proj2 type [simple]" in {
//    Proj2(DPair(fuu, fuu)) should haveTypeInContext(envWithUnit, "Unit")
//  }
//
//  it should "infer proj2 type [levels]" in {
//    Proj2(DPair(Level(0), Level(0))) should haveTypeInContext(Map(), Level(1))
//  }
//
//  it should "infer proj2 type [simple][2]" in {
//    Proj2(DPair("Unit", "Unit")) should haveTypeInContext(envWithUnit, Level(0))
//  }

  it should "break dependent pairs" in {
    Break(DPair(fuu, fuu), "f", "s", "f") should haveTypeInContext(envWithUnit, "Unit")
  }

  it should "break dependent pairs 2" in {
    val UUU = Sigma(Abs("qq", "Unit", "qq".ccaset(Map("uu" -> "Unit"), Level(0))))
    val uuu = DPair(fuu, fuu, UUU)
    val exType = "www".pi("UUU", "Unit")
    val ex = "pp".lam("UUU", "pp".break("f", "s",
        "f".ccaset(Map("uu" -> "s"), "Unit")
    ))
    val eenv: Environment = Map(
      vv("UUU") -> auto(UUU),
      vv("uuu") -> auto(uuu),
      vv("ex") -> EnvValue(ex, exType)
    )
    val env: Environment = envWithUnit ++ envWithBBool ++ eenv
    uuu should haveTypeInContext(env, UUU)
    ex should haveTypeInContext(env, exType)
    ex.app("uuu") should haveTypeInContext(env, "Unit")
//    "ex".app("uuu") should haveTypeInContext(env, "Unit") // TODO weird, uncommenting results in a crash
  }

  it should "break dependent pairs 3" in {
    val BBB = Sigma(Abs("qq", "Bool", "qq".ccaset(Map(
      "ff" -> "Unit",
      "tt" -> "Bool"), Level(0))))
    val bbb = DPair(ftt, fff, "BBB")
    val eenv: Environment = Map(
      vv("BBB") -> auto(BBB),
      vv("bbb") -> auto(bbb)
    )
    val env: Environment = envWithUnit ++ envWithBBool ++ eenv
    Var("bbb") should haveTypeInContext(env, "BBB")
    Break(bbb, "f", "s", "f") should haveTypeInContext(env, "Bool")
  }

  it should "kinda functor" in {
    val env = envWithUnit ++ extendedBoolEnv ++ envWithMaybeBool
    bfmap should haveTypeInContext(env, bfmapType)
    val jst = BJust.app(fff) // Just False
    bfmap.app(notTerm, jst) should haveTypeInContext(env, "MaybeBool")
  }
}
