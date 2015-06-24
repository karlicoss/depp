package terms.erase

case class ETuple(a: EType, b: EType) extends EType { // TODO EType?
  def codegen(): String = {
    val ta: String = null // translated a
    val tb: String = null // translated b
    s"{ $ta, $tb }"
  }
}
