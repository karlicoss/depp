package typecheck.inference


import terms.{Case, Term, Level, Finite}
import terms.Variables._
import typecheck.Environment.EnvValue
import util.Implicits._

import scala.collection.Map
import scala.collection.immutable.{Map => IMap}

/**
 * Polymorphic identity function context
 */
object IdContext {
  val pidType = "X".pi(Level(0), "x".pi("X", "X"))
  val pid = ".".lam("x".lam("x"))

  val envWithPid = IMap(vv("pid") -> EnvValue(pidType, pid))
}

