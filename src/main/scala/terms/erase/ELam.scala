package terms.erase

import terms.Variables.Variable

case class ELam(x: Variable, tp: ETerm, body: ETerm) extends ETerm { // TODO EType?

}
