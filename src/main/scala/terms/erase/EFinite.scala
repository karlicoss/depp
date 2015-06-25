package terms.erase

import terms.FElem
import terms.FElem.FElemType

case class EFinite(fname: String, elems: List[FElem.FElemType]) extends EType {

  def getIndex(e: FElemType): Int = elems.indexOf(e)

//  case class Code(tp: String, elems: List[String])
}
