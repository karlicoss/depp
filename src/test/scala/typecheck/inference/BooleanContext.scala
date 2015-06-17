package typecheck.inference

import terms.{Case, Term, Level, Finite}
import terms.Variables._
import typecheck.Environment.EnvValue
import util.Implicits._

object BooleanContext {
  val BBoolean = Finite(Set("tt", "ff"))

  val envWithBBoolean = Map(vv("Bool") -> EnvValue(Level(0), BBoolean))

  def bif(cond: Term, th: Term, el: Term): Term = {
    val cc = Map(
      vv("tt") -> th,
      vv("ff") -> el
    )
    Case(cond, cc, th)
  }
}
