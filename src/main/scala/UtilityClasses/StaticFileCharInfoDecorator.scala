package UtilityClasses

trait StaticFileCharInfoDecorator extends StaticFileCharInfo {
  def decorated: StaticFileCharInfo

  override val grapheme: Grapheme = decorated.grapheme
  override val conwayColl: ConwayColl = decorated.conwayColl
  override val ids: List[Cluster] = decorated.ids

  override def hashCode(): Int = decorated.hashCode()
  override def equals(obj: Any): Boolean = decorated.equals(obj)
}
