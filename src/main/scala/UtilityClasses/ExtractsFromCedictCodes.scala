package UtilityClasses

enum ExtractsFromCedictCodes(val indices: List[Int]) {
  case FirstToFifthAndLast extends ExtractsFromCedictCodes(List(2, 2, 2, 2, 2, -2))
  case FirstSecondThirdLast extends ExtractsFromCedictCodes(List(2, 2, 2, -2))
  case FirstSecondLast extends ExtractsFromCedictCodes(List(2, 2, -2))
  case FirstLast extends ExtractsFromCedictCodes(List(2, -2))
  case FirstOnly extends ExtractsFromCedictCodes(List(2))
}