package terms

import terms.Abstraction.Abs
import terms.Variables.Simple
import typecheck.Environment.Environment

import scalaz.State

case class Sigma(abs: Abs) extends Term {
  override def pretty(): String = "Exists " + abs.pretty() // TODO

  /**
   * Infers the type of the expression under the given context
   * @param env the context
   * @return
   */
  override def inferHelper(env: Environment): State[Int, Term] = State.state(Common.inferPiSigma(abs, env))

  override def substHelper(env: Environment): State[Int, Term] = for {
    res <- abs.substHelper(env)
  } yield Sigma(res)

  /**
   * Evaluates the expression under the given context
   * @param env the context
   * @return
   */
  override def evaluate(env: Environment): Term = Sigma(abs.evaluate(env))
}

object Sigma {
  def create(name: String, tp: Term, body: Term): Sigma = Sigma(Abs(Simple(name), tp, body))
}