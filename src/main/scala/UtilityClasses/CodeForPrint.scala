package UtilityClasses

class CodeForPrint(val code: String) {
  require(code.forall(s => s.isLower && s >= 'a' && s <= 'z') && code.nonEmpty && code.length <= 6)

  // Override equals
  override def equals(obj: Any): Boolean = obj match {
    case that: CodeForPrint => this.code == that.code
    case _ => false
  }

  // Override hashCode
  override def hashCode(): Int = code.hashCode()
  
}
