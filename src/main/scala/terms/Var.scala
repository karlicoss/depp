package terms

import terms.Variables.{Simple, Variable}
import typecheck.Environment.{EnvValue, Environment}
import typecheck.inference.TypeInferenceException
import util.Utils.toInstance

import scalaz.State

final case class Var(name: Variable) extends Term {
  override def pretty(): String = name.pretty()

  override def evaluate(env: Environment): Term = {
    env.get(name) match {
      case Some(x) => this
      case None => searchFinite(env) match {
        case Some(x) =>
          this // TODO ??
        case None =>
          throw TypeInferenceException(s"Unknown variable ${name.pretty()}") // TODO EvaluationException
      }
    }
  }

  override def substHelper(env: Environment) = State.state {
    env get name match {
      case Some(x) => x.tp // TODO ????
      case None => Var(name)
    }
  }

  private def searchFinite(env: Environment): Option[Variable] = {
    def aaa(v: Variable, e: EnvValue): Option[Variable] = for {
      df <- e.dfn
      finite <- toInstance[Finite](df)
      if finite.elems.contains(this.name)
    } yield v
    env.toSeq.flatMap(p => aaa(p._1, p._2)).headOption
  }

  override def infer(env: Environment): Term = env.get(name) match {
    case Some(x) => x.tp
    case None => searchFinite(env) match {
      case Some(x) => Var(x)
      case None => throw TypeInferenceException(s"Unbound variable ${name.pretty()}")
    }
  }
}

object Var {
  def simple(name: String): Var = Var(Simple(name))
}