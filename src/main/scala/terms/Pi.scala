package terms

import terms.Abstraction.Abs
import terms.Variables.Simple
import typecheck.Environment._

import scalaz.State

final case class Pi(abs: Abs) extends Term {
  override def pretty(): String = "Ɐ" + abs.pretty()

  override def evaluate(env: Environment): Term = Pi(abs.evaluate(env))

  override def substHelper(env: Environment) = for {
    res <- abs.substHelper(env)
  } yield Pi(res)

  override def inferHelper(env: Environment): State[Int, Term] = State.state(Common.inferPiSigma(abs, env))
}

object Pi {
  def create(name: String, tp: Term, body: Term): Pi = Pi(Abs(Simple(name), tp, body))
}