package terms

import terms.Abstraction.Abs
import typecheck.Beta
import typecheck.Environment._
import typecheck.inference.{Inference, TypeInferenceException}
import util.Implicits.type2EnvElem

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

    def inferPi(env: Environment, term: Term): Abs = {
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

    def assumeEqual(env: Environment, t1: Term, t2: Term): Unit = {
      if (!Beta.equivalent(env, t1, t2)) {
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
