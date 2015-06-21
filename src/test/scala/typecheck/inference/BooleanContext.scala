package typecheck.inference

import terms._
import terms.Variables._
import typecheck.Environment.EnvValue
import util.Implicits._

import scala.collection.Map
import scala.collection.immutable.{Map => IMap}

object BooleanContext {
  /**
   * BBool = {tt, ff}
   */
  val BBool = Finite(Set("tt", "ff"))

  /**
   * data Bool = {tt, ff}
   */
  val envWithBBool = IMap(
    vv("Bool") -> EnvValue(Level(0), BBool)
  )

  /*
    if cond then th else el
   */
  def bif(cond: Term, th: Term, el: Term): Term = {
    val cc = Map(
      "tt" -> th,
      "ff" -> el
    )
    Case(cond, cc)
  }

  /**
   * if = \cond.\then.\else.case (cond) of {tt -> then; ff -> else}
   */
  val ifTerm = "cond".lam("then".lam("else".lam(bif("cond", "then", "else"))))

  /**
   * and = \a.\b.if (a) then b else false
   */
  val andTerm = "a".lam("b".lam("if".app("a", "b", "ff")))

  val extendedBoolEnv = IMap(
    vv("Bool") -> EnvValue(TVar.dummy, BBool),
    vv("if") -> EnvValue(TVar.dummy, ifTerm),
    vv("and") -> EnvValue(TVar.dummy, andTerm)
  )
}
