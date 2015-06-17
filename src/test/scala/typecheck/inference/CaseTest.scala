package typecheck.inference

import com.sun.org.apache.xpath.internal.operations.Variable
import terms.Variables.vv

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
    bif("tt", "ff", "tt") should haveTypeInContext(envWithBBoolean, "Bool")
  }

  it should "evaluate if-then-else-clause" in {
    bif("tt", "ff", "tt") should beBequivalentTo(envWithBBoolean, "ff")
  }

  val alalaEnv = envWithBBoolean ++ envWithUnit

  it should "infer type of wuut" in {
    val tp = Sigma(Abs(".", "Bool", bif(".", "Unit", "Bool")))
    tp should haveTypeInContext(alalaEnv, Level(0))
  }

  it should "infer type of fsfsd" in {
    // Σ Unit (λ { unit → Bool })
    val qq = Sigma(Abs(".", "Unit", Var(".").ccase(
      IMap(
        vv("unit") -> "Bool"),
      "Bool")))
    qq should haveTypeInContext(alalaEnv, Level(0))
  }
}
