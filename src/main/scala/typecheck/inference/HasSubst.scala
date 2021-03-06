package typecheck.inference

import typecheck.Environment.Environment

import scalaz.State

trait HasSubst[T] {
  /**
   * Performs a substition using the given context
   *
   * TODO: substitution environment is not the same thing as type inference/evaluation environment
   *
   * @param env the context
   * @return
   */
  def subst(env: Environment): T = substHelper(env).eval(0)

  def substHelper(env: Environment): State[Int, T] // Int is the variables counter
}
