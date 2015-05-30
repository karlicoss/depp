package typecheck.inference

import java.lang.Math.max

import terms.Abstraction.Abs
import terms.Terms._
import terms.Variables.Variable

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
          printEquation(abs.v.pretty(), vid)
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

  def evaluate(env: Map[Variable, Term], term: Term): Term = {
    def evaluateAbs(env: Map[Variable, Term], abs: Abs): Abs = {
      val etp = evaluate(env, abs.tp)
      val ebody = evaluate(env + (abs.v -> etp), abs.body)
      Abs(abs.v, etp, ebody)
    }

    term match {
      case Var(name) => env.get(name) match {
        case Some(x) => Var(name)
        case None => throw TypeInferenceException(s"Unknown variable ${name.pretty()}") // TODO EvaluationException
      }
      case Level(kind) => Level(kind)
      case Lam(abs) => Lam(evaluateAbs(env, abs))
      case Pi(abs) => Pi(evaluateAbs(env, abs))
      case App(a, b) => {
        val arg = evaluate(env, b)
        val fn = evaluate(env, a)
        fn match {
          case Lam(abs) => evaluate(env, abs.body.subst(Map(abs.v -> arg)))
          case _ => App(arg, fn)
        }
      }
    }
  }

  def infer(env: Map[Variable, Term], term: Term): Term = {
    def assumeEqual(env: Map[Variable, Term], t1: Term, t2: Term): Unit = {
      if (!equal(env, t1, t2)) {
        throw TypeInferenceException("TODO")
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

    term match {
      case Var(name) => env.get(name) match {
        case Some(x) => x
        case None => throw TypeInferenceException(s"Unbound variable ${name.pretty()}")
      }
      case Level(level) => Level(level + 1)
      case Lam(abs) => {
        val level = inferLevel(env, abs.tp)
        val tp = infer(env + (abs.v -> abs.tp), abs.body)
        Pi(Abs(abs.v, abs.tp, tp))
      }
      case Pi(abs) => {
        val level1 = inferLevel(env, abs.tp)
        val level2 = inferLevel(env + (abs.v -> abs.tp), abs.body)
        Level(max(level1, level2))
      }
      case App(a, b) => {
        val abs = inferPi(env, a)
        val argType = infer(env, b)
        assumeEqual(env, abs.tp, argType)
        abs.body.subst(Map(abs.v -> b))
      }
    }
  }

  /**
   * Beta equivalence in context
   */
  def equal(env: Map[Variable, Term], t1: Term, t2: Term): Boolean = {
    def helper(t1: Term, t2: Term): Boolean = {
      (t1, t2) match {
        case (Var(a), Var(b)) => a == b
        case (Level(a), Level(b)) => a == b
        case (Lam(aabs), Lam(babs)) => absHelper(aabs, babs)
        case (Pi(aabs), Pi(babs)) => absHelper(aabs, babs)
        case (App(a, b), App(c, d)) => helper(a, c) && helper(b, d)
      }
    }

    def absHelper(a: Abs, b: Abs): Boolean = {
      helper(a.tp, b.tp) && helper(a.body, b.body.subst(Map(b.v -> Var(a.v))))
    }

    helper(evaluate(env, t1), evaluate(env, t2))
  }
}