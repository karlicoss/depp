package typecheck.inference

import typecheck.Environment.Environment

import scalaz.State

/**
 * Created by karlicos on 02.06.15.
 */
trait HasSubst[T] {
  /**
   * Performs a substition using the given context
   * @param env the context
   * @return
   */
  def subst(env: Environment): T = substHelper(env).eval(0)

  def substHelper(env: Environment): State[Int, T] // Int is the variables counter
}
