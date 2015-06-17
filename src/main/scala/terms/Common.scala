package terms

import java.lang.Math._

import terms.Abstraction.Abs
import typecheck.Environment._
import typecheck.inference.TypeInferenceException
import util.Implicits.type2EnvElem

object Common {
  def inferPiSigma(abs: Abs, env: Environment): Term = {
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

  def inferSigma(env: Environment, term: Term): Abs = {
    val funType = term.infer(env)
    val ev = funType.evaluate(env)
    ev match {
      case Sigma(abs) => abs
      case _ => {
        val message = s"Expected ${funType.pretty()} to be evaluated in Sigma type, got ${ev.pretty()} instead"
        throw TypeInferenceException(message)
      }
    }
  }
}
