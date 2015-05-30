package types.inference

import java.lang.Math.max

import terms.Abstraction.Abs
import terms.Terms._
import terms.Variables.Variable
import typecheck.Substitution.subst

/**
 * Created by karlicos on 30.05.15.
 */
package object Inference {
  /**
   * Prints out a system of type equations for the given term
   */
  def alala(term: Term): Unit = {
    var v = 0
    def next_id(): String = {
      val res = v
      v += 1
      return s"t$res"
    }
    def getArrow(a: String, b: String): String = s"$a -> $b"

    def printEquation(term: String, hypot: String): Unit = {
      println(s"$term :: $hypot")
    }

    def helper(term: Term): Unit = {
      term match {
        case Var(name) => {
          val id = next_id()
          printEquation(term.pretty(), id)
        }
        case Lam(abs) => {
          val vid = next_id()
          val bid = next_id()
          printEquation(abs.name.pretty(), vid)
          printEquation(abs.body.pretty(), bid)
          printEquation(abs.tp.pretty(), getArrow(vid, bid))
          helper(abs.body)
        }
        case App(a, b) => {
          val appid = next_id()
          val argid = next_id()
          printEquation(a.pretty(), getArrow(argid, appid))
          printEquation(b.pretty(), argid)
          printEquation(term.pretty(), appid)
          helper(a)
          helper(b)
        }
//        case Prod(terms) => {
//          println("TODO")
//        }
      }
    }
    helper(term)
  }

  def evaluate(map: Map[Variable, Term], term: Term): Term = ???

  /**
   * TODO: mutable context?
   */
  def infer(env: Map[Variable, Term], term: Term): Term = {
    term match {
      case Var(name) => env.get(name) match {
        case Some(x) => x
        case None => ??? // TODO raise error
      }
      case Level(level) => Level(level + 1)
      case Lam(abs) => {
        val level = inferLevel(env, abs.tp)
        val tp = infer(env + (abs.name -> abs.tp), abs.body)
        Pi(Abs(abs.name, abs.tp, tp))
      }
      case Pi(abs) => {
        val level1 = inferLevel(env, abs.tp)
        val level2 = inferLevel(env + (abs.name -> abs.tp), abs.body)
        Level(max(level1, level2))
      }
      case App(a, b) => {
        val abs = inferPi(env, a)
        val argType = infer(env, b)
        // TODO check argType and abs.tp for equality
        subst(Map(abs.name -> b), abs.body)
      }
    }
  }

  def inferPi(env: Map[Variable, Term], term: Term): Abs = {
    val funType = infer(env, term)
    val ev = evaluate(env, funType)
    ev match {
      case Pi(abs) => abs
      case _ => {
        val message = s"Expected ${funType.pretty()} to be evaluated in Pi type, got ${ev.pretty()} instead"
        throw TypeInferenceException(message)
      }
    }
  }

  def inferLevel(env: Map[Variable, Term], term: Term): Integer = {
    val tp = infer(env, term)
    val ev = evaluate(env, tp)
    ev match {
      case Level(kind) => kind
      case _ => {
        val message = s"Expected ${tp.pretty()} to be evaluated in Level, got ${ev.pretty()} instead"
        throw TypeInferenceException(message)
      }
    }
  }
}
