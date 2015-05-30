package terms

import util.PrettyPrintable

/**
 * Created by karlicos on 30.05.15.
 */
package object Variables {
  sealed abstract class Variable extends PrettyPrintable

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

  def vv(name: String): Variable = Simple(name)
}
