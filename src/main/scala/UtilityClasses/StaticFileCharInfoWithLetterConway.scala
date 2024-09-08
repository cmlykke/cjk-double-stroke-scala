package UtilityClasses

class StaticFileCharInfoWithLetterConway(val decorated: StaticFileCharInfo,
                                         val InputLetterConway: Set[ConwayUnambigous])
  extends StaticFileCharInfo(decorated.grapheme, decorated.conwayColl, decorated.ids)
    with StaticFileCharInfoDecorator {

  val letterConway: Set[ConwayUnambigous] = InputLetterConway

}