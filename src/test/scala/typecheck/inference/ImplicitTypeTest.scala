package typecheck.inference

import terms.Variables.{Dummy, vv}
import terms._
import typecheck.CustomMatchers
import typecheck.Environment._
import util.Implicits._
import util.UnitSpec

import scala.collection.immutable.{Map => IMap}


class ImplicitTypeTest extends UnitSpec with CustomMatchers {

  val env: Environment = IMap(
    vv("A") -> Level(0),
    vv("a") -> Var("A")
  )

  def hasDummy(a: Abs): Boolean = a.v.isInstanceOf[Dummy] || hasDummy(a.tp) || hasDummy(a.body)

  def hasDummy(t: Term): Boolean = t match {
    case Level(kind) => false
    case Proj1(trm) => hasDummy(trm)
    case Proj2(trm) => hasDummy(trm)
    case Finite(elems) => false
    case Pi(abs) => hasDummy(abs)
    case Sigma(abs) => hasDummy(abs)
    case TVar(v) => v.isInstanceOf[Dummy]
    case Lam(abs) => hasDummy(abs)
    case Case(cond, cases, dflt) =>
      hasDummy(cond) ||
        cases.values.map(hasDummy).exists(_ == true) ||
        (dflt match {
          case Some(x) => hasDummy(x)
          case None => false
        })
    case DPair(a, b, tp) => hasDummy(a) || hasDummy(b) || hasDummy(tp)
    case App(a, b) => hasDummy(a) || hasDummy(b)
    case Var(name) => name.isInstanceOf[Dummy]
    case _ => ???
  }

  it should "should remove dummy types after inference" in {
    val tp = ("x".lam("x")).infer(env)
    hasDummy(tp) should be (false)
  }

  it should "infer implicit type based on argument type" in {
    ("x".lam("x")).app("a") should haveTypeInContext(env, "A")
  }

  it should "meh" in {
    ("x".lam(("y".lam("A", "y")).app("x"))).app("a") should haveTypeInContext(env, "A")
  }

  /**
   * Type inference: a very restricted sumset
   *
   * (\x:Dummy. body) (arg: Arg) : Arg should be immediately substituted instead of Dummy
   * , BEFORE inferring the body type.
   *
   *
   *
   */
}
