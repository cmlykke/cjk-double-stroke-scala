
package UtilityClasses

class StaticFileCharInfo(val grapheme: Grapheme,
                         val conwayColl: ConwayColl,
                         val ids: List[Cluster]) {

  override def hashCode(): Int = {
    grapheme.hashCode()
  }

  override def equals(obj: Any): Boolean = {
    obj match {
      case other: StaticFileCharInfo => other.grapheme == this.grapheme
      case _ => false
    }
  }
}
