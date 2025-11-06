package Atypes

sealed trait AsortingCriteria {
  def code: Int // Common property for all implementations
}

enum SortingCodes(override val code: Int) extends AsortingCriteria {
  case NoCode extends SortingCodes(0)
  case OneCode extends SortingCodes(1)
  case TwoCode extends SortingCodes(2)
  case ThreeCodeTwoCharWord extends SortingCodes(3)
  case ThreeCodeSingleChar extends SortingCodes(4)
  case FourCode extends SortingCodes(5)
  case FiveCode extends SortingCodes(6)
  case SixCode extends SortingCodes(7)
}

enum PossibleWordCodes(override val code: Int) extends AsortingCriteria {
  case NoCodes extends PossibleWordCodes(0)
  case LastCode extends PossibleWordCodes(1)
  case FirstCode extends PossibleWordCodes(1)
  case FirstLastCode  extends PossibleWordCodes(2)
  case FirstSecondLastCode extends PossibleWordCodes(3)
  case FirstFirstFirstLastCode extends PossibleWordCodes(4)
  case FirstFirstFirstFirstLastCode extends PossibleWordCodes(5)
  case FirstFirstFirstFirstFirstLastCode extends PossibleWordCodes(6)
}

