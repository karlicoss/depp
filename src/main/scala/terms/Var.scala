package terms

import terms.Variables.{Dummy, Generated, Simple, Variable}
import typecheck.Environment.{EnvValue, Environment}
import typecheck.inference.TypeInferenceException
import util.Utils
import util.Utils.{toFinite, toInstance}

import scalaz.State

final case class Var(name: Variable) extends Term {
  override def pretty(): String = name.pretty()

  override def evaluate(env: Environment): Term = {
    env.get(name) match {
      case Some(EnvValue(tp, dfn)) => {
        dfn match {
          case Some(x) => x
          case None => this
        }
      }
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
      finite <- toFinite(df)
      if finite.elems.contains(this.name)
    } yield v
    env.toSeq.flatMap(p => aaa(p._1, p._2)).headOption
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
    case None => searchFinite(env) match {
      case Some(x) => State.state(Var(x))
      case None => throw TypeInferenceException(s"Unbound variable ${name.pretty()}")
    }
  }
}

object Var {
  def simple(name: String): Var = Var(Simple(name))
}