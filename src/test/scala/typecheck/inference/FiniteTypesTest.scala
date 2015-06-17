package typecheck.inference

import terms.{Var, Level, Finite}
import terms.Variables.Simple
import terms.Variables.vv
import typecheck.CustomMatchers
import typecheck.Environment.EnvValue
import util.Implicits._
import util.UnitSpec

class FiniteTypesTest extends UnitSpec with CustomMatchers {

  def makefinite(n: Integer): Finite = {
    val names = (0 until n).map(i => Simple("f" + i))
    Finite(names.toSet)
  }

  it should "infer type of empty finite type" in {
    makefinite(0) should haveTypeInContext(Map(), Level(0))
  }

  it should "infer type of one-element finite type" in {
    makefinite(1) should haveTypeInContext(Map(), Level(0))
  }

  it should "infer type of multiple element finite type" in {
    makefinite(10) should haveTypeInContext(Map(), Level(0))
  }

  it should "infer type of an element of finite type" in {
    val tp = makefinite(2) // f0, f1
    val env = Map(vv("Finite") -> EnvValue(Level(0), tp))
    Var("f0") should haveTypeInContext(env, "Finite")
    Var("f1") should haveTypeInContext(env, "Finite")
  }

  it should "evaluate elements of finite types" in {
    val tp = makefinite(1) // f0
    val env = Map(vv("Finite") -> EnvValue(Level(0), tp))
    Var("f0") should beBequivalentTo(env, "f0")
  }
}
