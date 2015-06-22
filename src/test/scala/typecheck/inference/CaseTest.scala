package typecheck.inference

import terms.Variables.vv
import terms._
import typecheck.CustomMatchers
import typecheck.Environment.EnvValue.auto
import typecheck.Environment.{Environment, EnvValue}
import typecheck.inference.BooleanContext._
import typecheck.inference.UnitPairContext._
import util.Implicits._
import util.UnitSpec

import scala.collection.immutable.{Map => IMap}


class CaseTest extends UnitSpec with CustomMatchers {
  val fff = FElem("ff")
  val ftt = FElem("tt")
  val fuu = FElem("uu")

  it should "infer type of if-then-else-clause" in {
    bif(ftt, fff, ftt, "Bool") should haveTypeInContext(envWithBBool, "Bool")
  }

  it should "evaluate if-then-else-clause" in {
    bif(ftt, fff, ftt, "Bool") should beBequivalentTo(envWithBBool, fff)
  }

  it should "trarar" in {
    val term = "a".lam("Unit", "a".ccaset(Map("uu" -> fuu), "Unit"))
    term should haveTypeInContext(envWithUnit, ".".pi("Unit", "Unit"))

    val env: Environment = envWithUnit + (vv("term") -> auto(term))
    Var("term") should haveTypeInContext(env, ".".pi("Unit", "Unit"))
  }

  val unitBoolEnv = envWithUnit ++ envWithBBool

  it should "infer type of wuut" in {
    val tp = Sigma(Abs(".", "Bool", bif(".", "Unit", "Bool", Level(0))))
    tp should haveTypeInContext(unitBoolEnv, Level(0))
  }

  it should "infer type of fsfsd" in {
    // Σ Unit (λ { unit → Bool })
    val qq = Sigma(Abs(".", "Unit", ".".ccaset(IMap("uu" -> "Bool"), Level(0))))
    qq should haveTypeInContext(unitBoolEnv, Level(0))
    DPair(fuu, ftt, qq)
  }

  it should "regerger" in {

    val topEnv = BooleanContext.topBotEnv
    // woo : (a : Unit) → (equ a unit → ⊥) → ⊥
    val statement = "a".pi("Unit", "qq".pi("ww".pi("equ".app("a", fuu), "Bot"), "Bot"))
    //  woo = λ { unit → λ z → z top }
    val proof = "x".lam("x".ccaset(IMap(
      "uu" -> "z".lam("z".app(FElem("top")))
    ), "qq".pi("ww".pi("equ".app("a", fuu), "Bot"), "Bot")))
    proof should haveTypeInContext(topEnv, statement)
  }

  /*
    data BTag : Set where
      BTEmpty : BTag
      BTJust  : BTag
   */
  val BTag = Finite(Set("BTEmpty", "BTJust"))
  val fBTEmpty = FElem("BTEmpty")
  val fBTJust = FElem("BTJust")

  // MaybeBool = Σ BTag (λ { BTEmpty → Unit ; BTJust → Bool })
  val MaybeBool = Sigma(Abs("t", "BTag", Var("t").ccaset(
    IMap(
      "BTEmpty" -> "Unit",
      "BTJust"  -> "Bool"),
    Level(0))))

  // BEmpty = (BTEmpty , unit)
  val BEmpty = DPair(fBTEmpty, fuu, "MaybeBool")

  // BJust = λ b → BTJust , b
  val BJust = "b".lam("Bool", DPair(fBTJust, "b", "MaybeBool"))

  val envWithMaybeBool = IMap(
    vv("BTag") -> EnvValue(Level(0), BTag),
    vv("MaybeBool") -> EnvValue(Level(0), MaybeBool),
    vv("BEmpty") -> EnvValue(TVar("xxx"), BEmpty), // TODO dummy type variables
    vv("BJust") -> EnvValue(TVar("yyy"), BJust)
  )

  it should "infer Empty :: MaybeBool" in {
    Var("BEmpty") should haveTypeInContext(unitBoolEnv ++ envWithMaybeBool, "MaybeBool")
  }

  it should "infer (Just false) :: MaybeBool, (Just true) :: MaybeBool" in {
    "BJust".app(fff) should haveTypeInContext(unitBoolEnv ++ envWithMaybeBool, "MaybeBool")
    "BJust".app(ftt) should haveTypeInContext(unitBoolEnv ++ envWithMaybeBool, "MaybeBool")
  }
}
