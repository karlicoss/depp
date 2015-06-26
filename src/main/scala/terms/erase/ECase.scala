package terms.erase

import terms.FElem.FElemType

import scala.collection.Map

case class ECase(cond: ETerm, cases: Map[FElemType, ETerm], dflt: Option[ETerm]) extends ETerm {

}
