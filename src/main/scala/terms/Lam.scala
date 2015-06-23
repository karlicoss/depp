package terms

import terms.Abs
import terms.Variables.{Generated, Variable, Dummy, Simple}
import terms.erase.{EType, ELam, ETerm}
import typecheck.Environment._
import util.Implicits.type2EnvElem

import scalaz.State

final case class Lam(abs: Abs) extends Term {
  override def pretty(): String = "λ" + abs.pretty()

  override def evaluate(env: Environment): Term = {
    Lam(abs.evaluate(env))
  }

  override def substHelper(env: Environment) = for {
    res <- abs.substHelper(env)
  } yield Lam(res)

  def isDummy(t: Term): Boolean = t match {
    case TVar(v) => v.isInstanceOf[Dummy]
    case _ => false
  }

  override def inferHelper(env: Environment): State[Int, Term] = for {
    ntp <- if (isDummy(abs.tp))
            for {
              i <- State.get[Int]
              _ <- State.modify[Int](_ + 1)
            } yield TVar(Generated("gen", i))
            else State.state[Int, Term](abs.tp)
    tp <- abs.body.inferHelper(env + (abs.v -> ntp))
  } yield Pi(Abs(abs.v, ntp, tp))

  override def erase(): Option[Either[ETerm, EType]] = for {
    tp <- abs.tp.erase()
    bd <- abs.body.erase()
  } yield Left(ELam(abs.v, tp.right.get, bd.left.get))
}

object Lam {
  def create(name: String, tp: Term, body: Term): Lam = Lam(Abs(Simple(name), tp, body))
}