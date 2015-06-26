package terms

import terms.Variables.{Dummy, Generated, Simple, Variable}
import typecheck.Environment.Environment
import typecheck.inference.{HasSubst, HasEvaluate}
import util.PrettyPrintable

import scalaz.State
import util.Implicits.type2EnvElem

/**
 * lambda v : tp.body
 */
/*
  TODO: implicit dummy http://stackoverflow.com/a/5828982/706389
 */
final case class Abs(v: Variable, var tp: Term, body: Term, dummy: Unit)
  extends PrettyPrintable with HasEvaluate[Abs] with HasSubst[Abs] {

  override def pretty(): String = s"${v.pretty()}:${tp.pretty()}.${body.pretty()}"

  override def evaluate(env: Environment): Abs = {
    val etp = tp.evaluate(env)
    val ebody = body.evaluate(env + (this.v -> etp))
    Abs(v, etp, ebody)
  }

  override def substHelper(env: Environment) = for {
    s <- State.get[Int]
    _ <- State.modify[Int](_ + 1)
    fv = v match {
      case Simple(name) => {
        Generated(name, s)
      }
      case Generated(name, id) => Generated(name, s)
      case Dummy() => null
    }
    resType <- tp.substHelper(env)
    resBody <- body.substHelper(env + (v -> Var(fv)))
  } yield Abs(fv, resType, resBody)
}

object Abs {
  /**
   * Constructs the abstraction; the argument type gets inferred from the type of the expression the abstraction is
   * applied to
   */
  def apply(v: Variable, tp: Term, body: Term): Abs = new Abs(v, tp, body, ())
  /*
    TODO: Should generate a fresh type variable?
    We should probably traverse the term and assign the type variables before the evaluation
   */
  def apply(v: Variable, body: Term): Abs = new Abs(v, TVar.dummy, body, ())
}