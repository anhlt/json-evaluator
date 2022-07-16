package io.jseval

case class SymbolTable(
    map: Map[String, SupportType],
    next: Option[SymbolTable]
) {

  def insert(key: String, supportType: SupportType): SymbolTable = {
    this.copy(next = this.next, map = map + (key -> supportType))
  }

  def lookup(key: String): Option[SupportType] = {

    map.get(key) match {
      case Some(value) => Some(value)
      case None        => next.flatMap(parrentScope => parrentScope.lookup(key))
    }

  }

  def enterScope() = SymbolTable(
    Map.empty[String, SupportType],
    Some(this)
  )

  def leaveScope(): Option[SymbolTable] = this.next

}
