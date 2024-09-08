package UtilityClasses

class ConwayUnambigous(val conwayPairs: List[String], val is4Code: Boolean) {

  //val conwayPairs: List[String] = input

  require(conwayPairs.forall(s =>
    (s.length == 1 && s.forall(c => c.isLetter)) ||
      (s.length == 1 && s.forall(c => c >= '1' && c <= '9')) ||
      (s.length == 2 && s.forall(c => c >= '1' && c <= '9'))
  ), "conwayPairs can contain strings of length one with a single alphabet character or strings of length one or two containing characters between '1' and '5'.")

  // Override equals
  override def equals(obj: Any): Boolean = obj match {
    case that: ConwayUnambigous => this.conwayPairs == that.conwayPairs
    case _ => false
  }

  // Override hashCode
  override def hashCode(): Int = conwayPairs.hashCode()

}
