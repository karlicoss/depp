package typecheck.inference

import com.sun.org.apache.xpath.internal.operations.Variable
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
import BooleanContext._


class CaseTest extends UnitSpec with CustomMatchers {
  it should "infer type of if-then-else-clause" in {
    bif("tt", "ff", "tt") should haveTypeInContext(envWithBBool, "Bool")
  }

  it should "evaluate if-then-else-clause" in {
    bif("tt", "ff", "tt") should beBequivalentTo(envWithBBool, "ff")
  }

  val alalaEnv = envWithBBool ++ envWithUnit

  it should "infer type of wuut" in {
    val tp = Sigma(Abs(".", "Bool", bif(".", "Unit", "Bool")))
    tp should haveTypeInContext(alalaEnv, Level(0))
  }

  it should "infer type of fsfsd" in {
    // Σ Unit (λ { unit → Bool })
    val qq = Sigma(Abs(".", "Unit", Var(".").ccase(IMap(vv("uu") -> "Bool"))))
    qq should haveTypeInContext(alalaEnv, Level(0))
    DPair("uu", "tt", qq)
  }

  /*
    data BTag : Set where
      BEmpty : BTag
      BJust  : BTag
   */
  val BTag = Finite(Set("BEmpty", "BJust"))

  // MaybeBool = Σ BTag (λ { BEmpty → Unit ; BJust → Bool })
  val MaybeBool = Sigma(Abs("t", "BTag", Var("t").ccase(
    IMap(
      vv("BEmpty") -> "Unit",
      vv("BJust")  -> "Bool"))))

  // BEmpty = (MEmpty , unit)
  val BEmpty = DPair("BEmpty", "unit", "MaybeBool")

  // BJust = λ b → MJust , b
  val BJust = "b".lam(DPair("BJust", "b", "MaybeBool"))

  val envWithMaybeBool = IMap(vv("MaybeBool") -> EnvValue(Level(0), MaybeBool))

  it should "alala maybebool" in {
    BEmpty should haveTypeInContext(alalaEnv ++ envWithMaybeBool, "MaybeBool")
  }
}
