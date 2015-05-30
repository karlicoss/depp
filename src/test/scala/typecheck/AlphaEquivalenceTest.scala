package typecheck

import terms.Abstraction.Abs
import terms.Terms._
import terms.Variables.vv
import util.UnitSpec



class AlphaEquivalenceTest extends UnitSpec with CustomMatchers {

  def simpleLambda(name: String, term: Term): Term = {
    Lam(Abs(vv(name), Level(0), term))
  }

  def makeId(name: String): Term = {
    simpleLambda(name, Var(vv(name)))
  }

  it should "treat as equivalent" in {
    val one = makeId("x")
    val two = makeId("y")

    one should beAequivalentTo(two)
  }

  it should "fsdfsdf" in {
    def alala(a: String, b: String): Term = {
      simpleLambda(a, simpleLambda(b, App(Var(vv(a)), Var(vv(b)))))
    }
    val xy = alala("x", "y")
    val yx = alala("y", "x")
    val yz = alala("y", "z")

    xy should beAequivalentTo(yx)
    xy should beAequivalentTo(yz)
  }

  it should "fefwef" in {
    val vx = vv("x")
    val vy = vv("y")
    val vz = vv("z")

    val term1 = simpleLambda("x", simpleLambda("y", App(Var(vy), Var(vy))))
    val term2 = simpleLambda("y", simpleLambda("y", App(Var(vy), Var(vy))))
    term1 should beAequivalentTo(term2)
    term2 should beAequivalentTo(term1)
  }

  it should "treat universes with same indices as eqivalent" in {
    Level(11) should beAequivalentTo(Level(11))
  }

  it should "treat universes with different indices as not equivalent" in {
    Level(1) should not (beAequivalentTo(Level(2)))
  }
}
