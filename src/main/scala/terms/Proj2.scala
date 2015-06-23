package terms

import terms.erase.{EProj1, EType, EProj2, ETerm}
import typecheck.Environment.Environment

import scala.util.Left
import scalaz.State
import util.Implicits._

case class Proj2(pair: Term) extends Term {
  /**
   * Infers the type of the expression under the given context
   * @param env the context
   * @return
   */
  override def inferHelper(env: Environment): State[Int, Term] = State.state {
    val abs = Common.inferSigma(env, pair)
    val first = Proj1(pair).evaluate(env) // TODO??
    abs.body.subst(Map(abs.v -> first)) // TODO ???
  }

  override def substHelper(env: Environment): State[Int, Term] = for {
    s <- pair.substHelper(env)
  } yield Proj2(s)

  /**
   * Evaluates the expression under the given context
   * @param env the context
   * @return
   */
  override def evaluate(env: Environment): Term = {
    val nt = pair.evaluate(env)
    nt match {
      case DPair(_, b, _) => {
        b
      }
      case _ => Proj2(nt)
    }
  }

  override def pretty(): String = toString

  override def erase(): Option[Either[ETerm, EType]] = for {
    pe <- pair.erase()
  } yield Left(EProj2(pe.left.get))
}
