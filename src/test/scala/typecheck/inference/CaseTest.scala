package typecheck.inference

import terms.Variables.vv
import typecheck.Environment.EnvValue

import scala.collection.Map
import scala.collection.immutable.{Map => IMap}

import terms.Abstraction.Abs
import terms._
import typecheck.CustomMatchers
import util.UnitSpec
import util.Implicits._

import UnitPairContext._
import typecheck.inference.BooleanContext._


class CaseTest extends UnitSpec with CustomMatchers {
  val fff = FElem("ff")
  val ftt = FElem("tt")
  val fuu = FElem("uu")

  it should "infer type of if-then-else-clause" in {
    bif(ftt, fff, ftt) should haveTypeInContext(envWithBBool, "Bool")
  }

  it should "evaluate if-then-else-clause" in {
    bif(ftt, fff, ftt) should beBequivalentTo(envWithBBool, fff)
  }

  val unitBoolEnv = envWithUnit ++ envWithBBool

  it should "infer type of wuut" in {
    val tp = Sigma(Abs(".", "Bool", bif(".", "Unit", "Bool")))
    tp should haveTypeInContext(unitBoolEnv, Level(0))
  }

  it should "infer type of fsfsd" in {
    // Σ Unit (λ { unit → Bool })
    val qq = Sigma(Abs(".", "Unit", Var(".").ccase(IMap("uu" -> "Bool"))))
    qq should haveTypeInContext(unitBoolEnv, Level(0))
    DPair(fuu, ftt, qq)
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
  val MaybeBool = Sigma(Abs("t", "BTag", Var("t").ccase(
    IMap(
      "BTEmpty" -> "Unit",
      "BTJust"  -> "Bool"))))

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
