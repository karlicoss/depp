package typecheck

import typecheck.Environment.Environment

/**
 * Created by karlicos on 02.06.15.
 */
trait HasInference[T] {
  /**
   * Infers the type of the expression under the given context
   * @param env the context
   * @return
   */
  def infer(env: Environment): T
}
