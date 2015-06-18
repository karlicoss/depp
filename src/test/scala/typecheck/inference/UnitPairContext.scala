package typecheck.inference

import terms.Abstraction.Abs
import terms.Variables._
import terms.{Level, Sigma, Term, Finite}
import util.Implicits._
import typecheck.Environment.EnvValue

import scala.collection.Map
import scala.collection.immutable.{Map => IMap}

object UnitPairContext {
  val Unit = Finite(Set("uu"))

  def PairType(A: Term, B: Term): Term = Sigma(Abs(".", A, B))

  val envWithUnit = IMap(vv("Unit") -> EnvValue(Level(0), Unit))

}
