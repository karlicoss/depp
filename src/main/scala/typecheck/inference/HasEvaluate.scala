package typecheck.inference

import typecheck.Environment.Environment

/**
 * Created by karlicos on 02.06.15.
 */
trait HasEvaluate[T] {
  def evaluate(env: Environment): T
}
