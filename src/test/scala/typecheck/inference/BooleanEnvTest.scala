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


/**
 * Various tests for Bools
 */
class BooleanEnvTest extends UnitSpec with CustomMatchers {
//  it should "ff : Bool, tt : Bool" in {
//    Var("ff") should haveTypeInContext(extendedBoolEnv, "Bool")
//    Var("tt") should haveTypeInContext(extendedBoolEnv, "Bool")
//  }
//
//  it should "(if tt then ff else tt) -> ff" in {
//    Var("if").app("tt", "ff", "tt") should beBequivalentTo(extendedBoolEnv, "ff")
//  }

  it should "and function tests" in {
    Var("and").app("tt", "tt") should beBequivalentTo(extendedBoolEnv, "tt")
//    Var("and").app("tt", "ff") should beBequivalentTo(extendedBoolEnv, "ff")
//    Var("and").app("ff", "tt") should beBequivalentTo(extendedBoolEnv, "ff")
//    Var("and").app("ff", "ff") should beBequivalentTo(extendedBoolEnv, "ff")
  }
}
