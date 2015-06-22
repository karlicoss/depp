package parser

class ParserException private(message: String) extends RuntimeException(message)

object ParserException {
  def apply(message: String) = new ParserException(message)
  def apply(message: String, cause: Throwable) = new ParserException(message).initCause(cause)
}
