package typecheck.inference

import terms.Variables._
import terms._
import typecheck.Environment.EnvValue
import util.Implicits._

import scala.collection.immutable.{Map => IMap}

object UnitPairContext {
  val Unit = Finite(Set("uu"))
  val fuu = FElem("uu")

  def PairType(A: Term, B: Term): Term = Sigma(Abs(".", A, B))

  val envWithUnit = IMap(vv("Unit") -> EnvValue(Level(0), Unit))

}
