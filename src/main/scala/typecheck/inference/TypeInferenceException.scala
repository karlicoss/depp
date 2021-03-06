package typecheck.inference

class TypeInferenceException private(message: String) extends RuntimeException(message)

object TypeInferenceException {
  def apply(message: String) = new TypeInferenceException(message)
  def apply(message: String, cause: Throwable) = new TypeInferenceException(message).initCause(cause)
}
