package terms

import java.lang.Math._

import terms.Abstraction.Abs
import terms.Variables.{Dummy, Simple, Variable}
import typecheck.Environment.Environment
import typecheck.{Beta, HasInference}
import typecheck.inference.{HasEvaluate, HasSubst, Inference, TypeInferenceException}
import util.PrettyPrintable

import scalaz.State

/**
 * Created by karlicos on 30.05.15.
 */

package object Terms {

  sealed abstract class Term
    extends PrettyPrintable with HasEvaluate[Term] with HasSubst[Term] with HasInference[Term] {

    def equal(other: Term): Boolean = Beta.equal(Map(), this, other)

    def app(other: Term): Term = App(this, other)

    /**
     * Replaces all occurences of dummy type variables with named
     */
    def undummy(): Term = {
      var count = 0

      def absHelper(abs: Abs): Abs = Abs(abs.v, helper(abs.tp), helper(abs.body))

      def helper(term: Term): Term = {
        term match {
          case Var(name) => Var(name)
          case TVar(v) => v match {
            case Dummy() =>
              val res = TVar(Simple(s"tv$count")) // TODO generated?
              count += 1
              res
            case _ => TVar(v)
          }
          case Lam(abs) => Lam(absHelper(abs))
          case Pi(abs) => Pi(absHelper(abs))
          case App(a, b) => App(helper(a), helper(b))
          case Level(kind) => Level(kind)
        }
      }

      helper(this)
    }
  }

  object Var {
    def simple(name: String): Var = Var(Simple(name))
  }

  final case class Var(name: Variable) extends Term {
    override def pretty(): String = name.pretty()

    override def evaluate(env: Environment): Term = {
      env.get(name) match {
        case Some(x) => this
        case None => throw TypeInferenceException(s"Unknown variable ${name.pretty()}") // TODO EvaluationException
      }
    }

    override def substHelper(env: Environment) = State.state {
      env get name match {
        case Some(x) => x
        case None => Var(name)
      }
    }

    override def infer(env: Environment): Term = env.get(name) match {
      case Some(x) => x
      case None => throw TypeInferenceException(s"Unbound variable ${name.pretty()}")
    }
  }

  object Lam {
    def create(name: String, tp: Term, body: Term): Lam = Lam(Abs(Simple(name), tp, body))
  }

  final case class Lam(abs: Abs) extends Term {
    override def pretty(): String = "λ" + abs.pretty()

    override def evaluate(env: Environment): Term = Lam(abs.evaluate(env))

    override def substHelper(env: Environment) = for {
      res <- abs.substHelper(env)
    } yield Lam(res)

    override def infer(env: Environment): Term = {
      // at this point, the type of abstraction should be inferred. Right?
      val tp = abs.body.infer(env + (abs.v -> abs.tp))
      Pi(Abs(abs.v, abs.tp, tp))
    }
  }

  final case class App(a: Term, b: Term) extends Term {
    override def pretty(): String = {
      (a, b) match {
        case (Lam(abs), _) => s"(${a.pretty()}) ${b.pretty()}"
        case (_, Var(v))   => s"${a.pretty()} ${b.pretty()}"
        case _             => s"${a.pretty()} (${b.pretty()})"
      }
    }

    // TODO poorly tested
    override def evaluate(env: Environment): Term = {
      val fn = a.evaluate(env)
      val arg = b.evaluate(env)
      fn match {
        case Lam(abs) => abs.body.subst(Map(abs.v -> arg)).evaluate(env)
        case _ => App(fn, arg)
      }
    }

    override def substHelper(env: Environment) = for {
      resa <- a.substHelper(env)
      resb <- b.substHelper(env)
    } yield App(resa, resb)

    override def infer(env: Environment): Term = {

      def inferPi(env: Map[Variable, Term], term: Term): Abs = {
        val funType = term.infer(env)
        val ev = funType.evaluate(env)
        ev match {
          case Pi(abs) => abs
          case _ => {
            val message = s"Expected ${funType.pretty()} to be evaluated in Pi type, got ${ev.pretty()} instead"
            throw TypeInferenceException(message)
          }
        }
      }

      def assumeEqual(env: Map[Variable, Term], t1: Term, t2: Term): Unit = {
        if (!Beta.equal(env, t1, t2)) {
          throw TypeInferenceException(
            s"Expected ${t1.pretty()} to be equal to ${t2.pretty()}")
        }
      }

      val argType = b.infer(env)
      val abs = inferPi(env, a)
      /*
          If the type of the bound variable is a type variable, we substitute the argument type for it, no checks

          If the type of the bound is supplied, we should check it against the argument
          // TODO check that it does not contain any type variables
       */
      val newabs = abs.tp match {
        case TVar(name) =>
          // TODO body substitution?
          Abs(abs.v, argType, Inference.substTv(name, argType, abs.body))
        case tp => {
          assumeEqual(env, tp, argType)
          abs
        }
      }

      newabs.body.subst(Map(abs.v -> b))
    }
  }

  object Pi {
    def create(name: String, tp: Term, body: Term): Pi = Pi(Abs(Simple(name), tp, body))
  }
  final case class Pi(abs: Abs) extends Term {
    override def pretty(): String = "Ɐ" + abs.pretty()

    override def evaluate(env: Environment): Term = Pi(abs.evaluate(env))

    override def substHelper(env: Environment) = for {
      res <- abs.substHelper(env)
    } yield Pi(res)

    override def infer(env: Environment): Term = {
      def inferLevel(env: Map[Variable, Term], term: Term): Integer = {
        val tp = term.infer(env)
        val ev = tp.evaluate(env)
        ev match {
          case Level(kind) => kind
          case _ => {
            val message = s"Expected ${tp.pretty()} to be evaluated in Level, got ${ev.pretty()} instead"
            throw TypeInferenceException(message)
          }
        }
      }

      val level1 = inferLevel(env, abs.tp)
      val level2 = inferLevel(env + (abs.v -> abs.tp), abs.body)
      Level(max(level1, level2))
    }
  }

  final case class Level(kind: Integer) extends Term {
    override def pretty(): String = s"Type$kind"

    override def evaluate(env: Environment): Term = this

    override def substHelper(env: Environment): State[Int, Term] = State.state(this)

    override def infer(env: Environment): Term = Level(kind + 1)
  }

  /**
   * let x = y in whatever is equivalent to (\x.whatever) y
   *
   * TODO type should be inferred?
   */
  final case class Let(v: Variable, tp: Term, what: Term, dummy: Unit) {
    def in(body: Term): Term = v.lam(tp, body).app(what)
  }

  object Let {
    def apply(v: Variable, tp: Term, what: Term): Let = new Let(v, tp, what, ())
    def apply(v: Variable, what: Term): Let = new Let(v, TVar.dummy, what, ())
  }

  final case class TVar(v: Variable) extends Term {
    override def pretty(): String = s"Tv${v.pretty()}"

    override def evaluate(env: Environment): Term = this // TODO??

    override def substHelper(env: Environment) = State.state(this) // TODO ???
    override def infer(env: Environment): Term =
      throw TypeInferenceException("TODO: should this case even be possible?")
  }

  object TVar {
    /**
     * TODO: dummy type variables have to be preprocessed
     */
    val dummy = TVar(Dummy())
  }
}


