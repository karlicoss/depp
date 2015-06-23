package terms.erase

trait HasErasure[T] {
  def erase(): Option[T]
}
