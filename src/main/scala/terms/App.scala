package terms

import terms.Variables.{Dummy, Generated, Simple}
import typecheck.Beta
import typecheck.Environment._
import typecheck.inference.{Inference, TypeInferenceException}
import util.Implicits.type2EnvElem

import scalaz.State

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
      case Lam(abs) => {
        val res = abs.body.subst(Map(abs.v -> EnvValue(arg)))
        res.evaluate(env)
      }
      case _ => App(fn, arg)
    }
  }

  override def substHelper(env: Environment) = for {
    resa <- a.substHelper(env)
    resb <- b.substHelper(env)
  } yield App(resa, resb)

  /**
   *
   * @param env
   * @param term
   * @param hint "typing hint"
   * @return
   */
  private def inferPi(env: Environment, term: Term, hint: Term): Abs = {
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

  private def assumeEqual(env: Environment, t1: Term, t2: Term): Unit = {
    if (!Beta.equivalent(env, t1, t2)) {
      throw TypeInferenceException(
        s"Expected ${t1.pretty()} to be equal to ${t2.pretty()}")
    }
  }

  override def inferHelper(env: Environment): State[Int, Term] = State.state {

    val argType = b.infer(env)
    val abs = inferPi(env, a, argType)

    var body: Term = null
    // if the type of the bound variable was not supplied, we just set it to the argument type
    // TODO we should probably make Dummy a separate term instead?
    abs.tp match {
      case TVar(name) => {
        // TODO check that name is not Dummy. It should probably be Generated only
        body = Inference.substTv(name, argType, abs.body)
      }
      case _ => {
        // now we should check the type of the bound variable against the argument type
        assumeEqual(env, abs.tp, argType)
        body = abs.body
      }
    }

    body.subst(Map(abs.v -> b))
  }
}
