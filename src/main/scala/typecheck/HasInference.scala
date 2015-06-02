package typecheck

import typecheck.Environment.Environment

/**
 * Created by karlicos on 02.06.15.
 */
trait HasInference[T] {
  def infer(env: Environment): T
}
