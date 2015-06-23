package terms

import terms.Variables.Simple
import terms.erase.{EType, EArrow, ETerm}
import typecheck.Environment._

import scalaz.State

final case class Pi(abs: Abs) extends Term {
  override def pretty(): String = "Ɐ" + abs.pretty()

  override def evaluate(env: Environment): Term = Pi(abs.evaluate(env))

  override def substHelper(env: Environment) = for {
    res <- abs.substHelper(env)
  } yield Pi(res)

  override def inferHelper(env: Environment): State[Int, Term] = Common.inferPiSigma(abs, env)

  override def erase(): Option[Either[ETerm, EType]] = for  {
    // TODO: all types should be in normal form!
    // TODO: we should only leave terms in Level(0)
    at <- abs.tp.erase()
    bt <- abs.body.erase()
  } yield Right(EArrow(at.right.get, bt.right.get))
}

object Pi {
  def create(name: String, tp: Term, body: Term): Pi = Pi(Abs(Simple(name), tp, body))
}