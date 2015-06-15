package terms

import terms.Variables.{Variable, Simple}
import typecheck.Environment._
import typecheck.inference.TypeInferenceException

import scalaz.State

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
      case Some(x) => x.tp // TODO ????
      case None => Var(name)
    }
  }

  override def infer(env: Environment): Term = env.get(name) match {
    case Some(x) => x.tp
    case None => throw TypeInferenceException(s"Unbound variable ${name.pretty()}")
  }
}

object Var {
  def simple(name: String): Var = Var(Simple(name))
}