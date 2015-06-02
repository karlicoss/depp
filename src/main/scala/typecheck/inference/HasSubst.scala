package typecheck.inference

import typecheck.Environment.Environment

/**
 * Created by karlicos on 02.06.15.
 */
trait HasSubst[T] {
  def subst(env: Environment): T = substHelper(env, 0)._1
  def substHelper(env: Environment, cnt: Int): (T, Int) // variables counter // TODO State monad
}
