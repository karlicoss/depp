package typecheck.inference

import terms.{Case, Term, Level, Finite}
import terms.Variables._
import typecheck.Environment.EnvValue
import util.Implicits._

import scala.collection.Map
import scala.collection.immutable.{Map => IMap}

object BooleanContext {
  val BBool = Finite(Set("tt", "ff"))

  val envWithBBool = IMap(vv("Bool") -> EnvValue(Level(0), BBool))

  /*
    if cond then th else el
   */
  def bif(cond: Term, th: Term, el: Term): Term = {
    val cc = Map(
      vv("tt") -> th,
      vv("ff") -> el
    )
    Case(cond, cc)
  }
}
