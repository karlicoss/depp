package terms.erase

import terms.Variables.Variable

case class ELam(x: Variable, tp: EType, body: ETerm) extends ETerm { // TODO EType?

}
