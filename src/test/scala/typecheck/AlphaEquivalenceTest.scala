package typecheck

import terms.Variables.vv
import terms._
import typecheck.Environment.{EnvValue, Environment}
import util.Implicits._
import util.Terms._
import util.UnitSpec

class AlphaEquivalenceTest extends UnitSpec with CustomMatchers {

  it should "treat identitiy functions as equivalent" in {
    val one = makeId("x")
    val two = makeId("y")

    one should beAequivalentTo(two)
  }

  it should "treat some complex expressions as a-equivalent" in {
    def alala(a: String, b: String): Term = {
      simpleLambda(a, simpleLambda(b, App(Var(vv(a)), Var(vv(b)))))
    }
    val xy = alala("x", "y")
    val yx = alala("y", "x")
    val yz = alala("y", "z")

    xy should beAequivalentTo(yx)
    xy should beAequivalentTo(yz)
  }

  it should "treat some complex expressions as a-equivalent 2" in {
    val vx = vv("x")
    val vy = vv("y")
    val vz = vv("z")

    val term1 = simpleLambda("x", simpleLambda("y", App(Var(vy), Var(vy))))
    val term2 = simpleLambda("y", simpleLambda("y", App(Var(vy), Var(vy))))
    term1 should beAequivalentTo(term2)
    term2 should beAequivalentTo(term1)
  }

  it should "fsfdsf" in {
    val env: Environment = Map(vv("x") -> Level(0), vv("y") -> Level(0))
    Var("x") shouldNot beAequivalentTo(env, Var("y"))
    Var("y") shouldNot beAequivalentTo(env, Var("x"))
  }

  it should "treat captured variables" in {
    val env: Environment = Map(vv("x") -> Level(0))
    val term1 = "x".lam("x")
    val term2 = "y".lam("x")
    term1 shouldNot beAequivalentTo(env, term2)
    term2 shouldNot beAequivalentTo(env, term1)
  }

  it should "treat universes with same indices as equivalent" in {
    Level(11) should beAequivalentTo(Level(11))
  }

  it should "treat universes with different indices as not equivalent" in {
    Level(1) shouldNot beAequivalentTo(Level(2))
  }

  it should "treat bound variables as equivalent" in {
    val ctx: Environment = Map(vv("X") -> Level(0))
    Var(vv("X")) should beAequivalentTo(ctx, Var(vv("X")))
  }

  val Unit = Finite(Set("unit"))
  val envWithUnit = Map(vv("Unit") -> EnvValue(Level(0), Unit))

  it should "treat simple pi types as alpha equivalent" in {
    val tp1 = Pi(Abs(".", "Unit", "Unit"))
    val tp2 = Pi(Abs("x", "Unit", "Unit"))
    tp1 should beAequivalentTo(envWithUnit, tp2)
  }

  it should "treat simple sigma types as alpha equivalent" in {
    val tp1 = Sigma(Abs(".", "Unit", "Unit"))
    val tp2 = Sigma(Abs("x", "Unit", "Unit"))
    tp1 should beAequivalentTo(envWithUnit, tp2)
  }


  val Alala = Finite(Set("one", "two"))
  val alalaEnv = Map(vv("Alala") -> EnvValue(Level(0), Unit))

  it should "treat same elements of finite types as equivalent" in {
    FElem("one") should beAequivalentTo(alalaEnv, FElem("one"))
    FElem("two") should beAequivalentTo(alalaEnv, FElem("two"))
  }

  it should "treat different elements of finite types as not equivalent" in {
    FElem("one") shouldNot beAequivalentTo(alalaEnv, FElem("two"))
    FElem("two") shouldNot beAequivalentTo(alalaEnv, FElem("one"))
  }
}
