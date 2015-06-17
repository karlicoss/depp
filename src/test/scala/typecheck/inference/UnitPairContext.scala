package typecheck.inference

import terms.Abstraction.Abs
import terms.Variables._
import terms.{Level, Sigma, Term, Finite}
import util.Implicits._
import typecheck.Environment.EnvValue

object UnitPairContext {
  val Unit = Finite(List("unit"))

  def PairType(a: Term, b: Term): Term = Sigma(Abs(".", a, b))

  val envWithUnit = Map(vv("Unit") -> EnvValue(Level(0), Unit))

}
