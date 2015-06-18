package typecheck.inference

import terms.{Level, Var}
import terms.Variables.vv
import typecheck.CustomMatchers
import typecheck.Environment.Environment
import util.Implicits._
import util.Terms._
import util.UnitSpec
import IdContext._
import typecheck.Environment.EnvValue

import scala.collection.Map
import scala.collection.immutable.{Map => IMap}

class SimpleInferenceTest extends UnitSpec with CustomMatchers {

  it should "infer definitions" in {
    val env = IMap(vv("A") -> EnvValue(Level(1), Level(0)))
    Var("A") should haveTypeInContext(env, Level(1))
  }

  it should "evaluate definitions" in {
    val env = IMap(vv("A") -> EnvValue(Level(1), Level(0)))
    Var("A") should beBequivalentTo(env, Level(0))
  }

  it should "infer type of polymorphic identity" in {
    pid should haveTypeInContext(envWithPid, pidType)
  }
}
