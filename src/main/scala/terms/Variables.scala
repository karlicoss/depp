package terms

import terms.Abstraction.Abs
import terms.Terms.Let
import util.PrettyPrintable

package object Variables {
  sealed abstract class Variable extends PrettyPrintable {
    def lam(tp: Term, body: Term): Lam = Lam(Abs(this, tp, body))

    def lam(body: Term): Lam = Lam(Abs(this, body))

    def pi(tp: Term, body: Term): Pi = Pi(Abs(this, tp, body))

    def let(tp: Term, what: Term): Let = Let(this, tp, what)

    def let(what: Term): Let = Let(this, what)
  }

  /**
   * The variable entered by the user
   */
  final case class Simple(name: String) extends Variable {
    override def pretty(): String = name
  }

  /**
   * The variable generated while substituting
   * @param name a hint for pretty printing
   */
  final case class Generated(name: String, id: Integer) extends Variable {
    override def pretty(): String = s"$name$id"
  }

  final case class Dummy() extends Variable {
    override def pretty(): String = "."
  }

  // final case class Whatever? for forall _:A. B

  def vv(name: String): Variable = Simple(name)
}
