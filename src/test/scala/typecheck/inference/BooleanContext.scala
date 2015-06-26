package typecheck.inference

import terms._
import terms.Variables._
import typecheck.Environment.EnvValue
import typecheck.Environment.EnvValue.auto
import util.Implicits._

import scala.collection.Map
import scala.collection.immutable.{Map => IMap}

object BooleanContext {
  /**
   * BBool = {ff, tt}
   */
  val BBool = Finite(Set("ff", "tt"))
  val fff = FElem("ff")
  val ftt = FElem("tt")

  /**
   * data Bool = {ff, tt}
   */
  val envWithBBool = IMap(
    vv("Bool") -> EnvValue(Level(0), BBool)
  )

  /*
    if cond then th else el

    TODO FIXME MAKE POLYMORPHIC!!!
   */
  def bif(cond: Term, th: Term, el: Term, tp: Term): Term = {
    val cc = Map(
      "tt" -> th,
      "ff" -> el
    )
    Case(cond, cc, None)
  }

  /**
   * if = \cond.\then.\else.case (cond) of {tt -> then; ff -> else}
   */
  val ifTerm = "cond".lam("Bool", "then".lam("Bool", "else".lam("Bool", bif("cond", "then", "else", "Bool"))))

  /**
   * and = \a.\b.if (a) then b else false
   */
  val andTerm = "a".lam("b".lam("if".app("a", "b", fff)))

  /**
   * not = \a. if(a) then false else true
   */
  val notTerm = "a".lam("Bool", "if".app("a", fff, ftt))

  /**
   * or = \a.\b.not ((not a) and (not b))
   */
  val orTerm = "a".lam("b".lam("not".app("and".app("not".app("a"), "not".app("b")))))


  /**
   * data Top : Set where
   *  top : Top
   */
  val topTerm = Finite(Set("top"))
  val ftop = FElem("top")

  /**
   * data Bot : Set where
   */
  val botTerm = Finite(Set())

  /**
   * neg = \statement. statement -> Bot
   */
  val negTerm = "S".lam("S".pi(TVar.dummy, "S".app("Bot")))

  /**
   * Booleans equality
   * tt eqb tt = Top
   * ff eqb ff = Top
   * _ eqb _ = Bot
   */
  val eqbTerm =
    "a".lam("Bool",
      "b".lam("Bool",
        "a".ccase(Map(
          "ff" -> "b".ccase(Map(
            "ff" -> "Top",
            "tt" -> "Bot"
          )),
          "tt" -> "b".ccase(Map(
            "ff" -> "Bot",
            "tt" -> "Top"
          ))
    ))))

  val topBotEnv = IMap(
    vv("Top") -> auto(topTerm),
    vv("Bot") -> auto(botTerm),
    vv("negTerm") -> auto(negTerm)
  )

  val extendedBoolEnv = IMap(
    vv("Bool") -> auto(BBool),
    vv("if") -> auto(ifTerm),
    vv("and") -> auto(andTerm),
    vv("not") -> auto(notTerm),
    vv("or") -> auto(orTerm),
    vv("eqb") -> auto(eqbTerm)
  ) ++ topBotEnv
}
