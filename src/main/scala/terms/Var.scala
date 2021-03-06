package terms

import terms.Variables.{Dummy, Simple, Variable}
import terms.erase.{EVar, EType, ETerm}
import typecheck.Environment.{EnvValue, Environment}
import typecheck.inference.TypeInferenceException

import scalaz.State

final case class Var(name: Variable) extends Term {
  override def pretty(): String = name.pretty()

  override def evaluate(env: Environment): Term = {
    env get name match {
      case Some(EnvValue(tp, dfn)) => {
        dfn match {
          case Some(x) => x // if there is a definition, evaluate the variable to it
          case None => this // otherwise, leave it intact
        }
      }
      case None => {
        throw TypeInferenceException(s"Unbound variable ${name.pretty()}") // TODO EvaluationException
      }
    }
  }

  override def substHelper(env: Environment) = State.state {
    env get name match {
      case Some(x) => x.tp // TODO ????
      case None => Var(name)
    }
  }

  override def inferHelper(env: Environment): State[Int, Term] = env.get(name) match {
    case Some(x) => x.tp match {
      case TVar(v) => x.dfn match {
        case Some(dfn) => dfn.inferHelper(env)
        case None => v match {
          case Dummy() => for {
            i <- State.get[Int]
            _ <- State.modify[Int](_ + 1) // generate new name
          } yield TVar(Simple(s"gen$i")) // TODO Simple -> Generated?
          case x => State.state(TVar(x)) // TODO well, we will probably infer it later?
        }
      }
      case other => State.state(other)
    }
    case None =>
      throw TypeInferenceException(s"Unbound variable ${name.pretty()}")
  }

  override def erase(): Option[Either[ETerm, EType]] = Some(Left(EVar(name.toString))) // TODO TOSTRING
}

object Var {
  def simple(name: String): Var = Var(Simple(name))
}