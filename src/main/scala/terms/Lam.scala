package terms

import terms.Abstraction.Abs
import terms.Variables.Simple
import typecheck.Environment._
import util.Implicits.type2EnvElem

import scalaz.State

final case class Lam(abs: Abs) extends Term {
  override def pretty(): String = "λ" + abs.pretty()

  override def evaluate(env: Environment): Term = Lam(abs.evaluate(env))

  override def substHelper(env: Environment) = for {
    res <- abs.substHelper(env)
  } yield Lam(res)

  override def inferHelper(env: Environment): State[Int, Term] = State.state {
    // at this point, the type of abstraction should be inferred. Right?
    val tp = abs.body.infer(env + (abs.v -> abs.tp))
    Pi(Abs(abs.v, abs.tp, tp))
  }
}

object Lam {
  def create(name: String, tp: Term, body: Term): Lam = Lam(Abs(Simple(name), tp, body))
}