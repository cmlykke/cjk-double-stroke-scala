package Atypes

sealed trait AsortingCriteria {
  def code: Int // Common property for all implementations
}

enum SortingCodes(override val code: Int) extends AsortingCriteria {
  case OneCode extends SortingCodes(1)
  case TwoCode extends SortingCodes(2)
  case TwoCodeLongChar extends SortingCodes(3)
  case ThreeCode extends SortingCodes(4)
  case ThreeCodeFullWord extends SortingCodes(5)
  case FourCode extends SortingCodes(6)
  case FiveCode extends SortingCodes(7)
  case SixCode extends SortingCodes(8)
}

enum PossibleWordCodes(override val code: Int) extends AsortingCriteria {
  case FirstCode extends PossibleWordCodes(1)
  case FirstLastCode  extends PossibleWordCodes(2)
  case FirstSecondLastCode extends PossibleWordCodes(3)
  case FirstFirstFirstLastCode extends PossibleWordCodes(4)
  case FirstFirstFirstFirstFirstLastCode extends PossibleWordCodes(6)
}