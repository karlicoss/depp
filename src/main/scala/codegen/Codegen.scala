package codegen

import terms.Variables.Variable
import terms.erase._
import typecheck.Environment.EnvValue

object Codegen {
  def genProgram(p: String): String = {
    p +
    """
      | define i32 @main() {
      |   ret i32 0
      | }
    """.stripMargin
  }

  def genType(tp: EType): String = ???

//  val program =
//    """
//      | Unit = { uu };
//      | id = fun u: Unit. u;
//      | id @uu
//    """.stripMargin

  def genEnvValue(v: Variable, tp: EType, dfn: ETerm): String = {
    val vname = v.pretty()
    tp match {
      case ESum(types) => ???
      case ETuple(a, b) => ???
      case EArrow(left, right) => ???
      case EFinite(name, elems) =>
      case _ => ???
    }

  }
}
