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
  it should "infer type of if-then-else-clause" in {
    bif("tt", "ff", "tt") should haveTypeInContext(envWithBBool, "Bool")
  }

  it should "evaluate if-then-else-clause" in {
    bif("tt", "ff", "tt") should beBequivalentTo(envWithBBool, "ff")
  }

  val unitBoolEnv = envWithUnit ++ envWithBBool

  it should "infer type of wuut" in {
    val tp = Sigma(Abs(".", "Bool", bif(".", "Unit", "Bool")))
    tp should haveTypeInContext(unitBoolEnv, Level(0))
  }

  it should "infer type of fsfsd" in {
    // Σ Unit (λ { unit → Bool })
    val qq = Sigma(Abs(".", "Unit", Var(".").ccase(IMap(vv("uu") -> "Bool"))))
    qq should haveTypeInContext(unitBoolEnv, Level(0))
    DPair("uu", "tt", qq)
  }

  /*
    data BTag : Set where
      BTEmpty : BTag
      BTJust  : BTag
   */
  val BTag = Finite(Set("BTEmpty", "BTJust"))

  // MaybeBool = Σ BTag (λ { BTEmpty → Unit ; BTJust → Bool })
  val MaybeBool = Sigma(Abs("t", "BTag", Var("t").ccase(
    IMap(
      vv("BTEmpty") -> "Unit",
      vv("BTJust")  -> "Bool"))))

  // BEmpty = (BTEmpty , unit)
  val BEmpty = DPair("BTEmpty", "uu", "MaybeBool")

  // BJust = λ b → BTJust , b
  val BJust = "b".lam("Bool", DPair("BTJust", "b", "MaybeBool"))

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
    "BJust".app("ff") should haveTypeInContext(unitBoolEnv ++ envWithMaybeBool, "MaybeBool")
    "BJust".app("tt") should haveTypeInContext(unitBoolEnv ++ envWithMaybeBool, "MaybeBool")
  }
}
