package terms

import java.lang.Math._

import terms.Abstraction.Abs
import terms.Variables.Simple
import typecheck.Environment._
import typecheck.inference.TypeInferenceException
import util.Implicits.type2EnvElem

final case class Pi(abs: Abs) extends Term {
  override def pretty(): String = "Ɐ" + abs.pretty()

  override def evaluate(env: Environment): Term = Pi(abs.evaluate(env))

  override def substHelper(env: Environment) = for {
    res <- abs.substHelper(env)
  } yield Pi(res)

  override def infer(env: Environment): Term = {
    def inferLevel(env: Environment, term: Term): Integer = {
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

object Pi {
  def create(name: String, tp: Term, body: Term): Pi = Pi(Abs(Simple(name), tp, body))
}