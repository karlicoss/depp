package terms

import terms.Variables.{Variable, Simple}
import terms.erase.{ETuple, ETerm}
import typecheck.Environment.Environment

import scalaz.State

case class Sigma(abs: Abs) extends Term {
  override def pretty(): String = "Exists " + abs.pretty() // TODO

  /**
   * Infers the type of the expression under the given context
   * @param env the context
   * @return
   */
  override def inferHelper(env: Environment): State[Int, Term] = Common.inferPiSigma(abs, env)

  override def substHelper(env: Environment): State[Int, Term] = abs.substHelper(env).map(Sigma(_))

  /**
   * Evaluates the expression under the given context
   * @param env the context
   * @return
   */
  override def evaluate(env: Environment): Term = Sigma(abs.evaluate(env))

  override def erase(): Option[ETerm] = for {
    ftype <- abs.tp.erase()
    stype <- abs.body.erase() // TODO weird..
  } yield ETuple(ftype, stype)
}

object Sigma {
  def create(name: Variable, tp: Term, body: Term): Sigma = Sigma(Abs(name, tp, body))
}