package terms.erase

import terms.FElem

case class EFinite(name: String, elems: List[FElem.FElemType]) extends EType {
  /**
   * Generates the code for the finite type definition
   * @return
   */
  def codegen(): String = {
    val tp =
      s"""
        |%$name = type { i32 }
      """.stripMargin
    val values: List[String] = for {
      (elem, i) <- elems.zipWithIndex
    } yield s"@$elem = internal global %$name { i32 $i }"
    s"; $name declaration" +
    (tp :: values).mkString("\n") + "\n" +
    s"; end of $name declaration" + "\n"
  }

//  case class Code(tp: String, elems: List[String])
}
