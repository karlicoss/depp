package typecheck.inference

import terms.Abstraction.Abs
import terms.Variables._
import terms.{Level, Sigma, Term, Finite}
import util.Implicits._
import typecheck.Environment.EnvValue

import scala.collection.Map
import scala.collection.immutable.{Map => IMap}

object UnitPairContext {
  val Unit = Finite(Set("unit"))

  def PairType(a: Term, b: Term): Term = Sigma(Abs(".", a, b))

  val envWithUnit = IMap(vv("Unit") -> EnvValue(Level(0), Unit))

}
