package terms.erase

import terms.Variables.Variable

case class EBreak(what: ETerm, f: Variable, s: Variable, body: ETerm) extends ETerm {

}
