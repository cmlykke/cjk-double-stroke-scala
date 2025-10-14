package AcodeGenerator

import AcodeGenerators.AgenerateSeudoCodes
import Adatasources.ManualData.AcodelengthRules
import Atypes.{AsortingCriteria, SortingCodes}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class AgenerateSeudoCodesTest extends AnyFlatSpec with Matchers {

  val codeMap: Map[String, String] = AcodelengthRules.elementTypes
  val fil: String = AcodelengthRules.fil

  it should "test that seudo letters can be generated from chars" in {
    val input1 = Set(
      (List("言", "135543"),4),
      (List("言", "135534"),4),
      (List("4111251135543"),6),
      (List("4111251135534"),6),
      (List("1111251135543"),6),
      (List("1111251135534"),6),
    )
    val seudo1: Set[(List[String], AsortingCriteria)] = AgenerateSeudoCodes.convertElemAndRemainderToSeudo(input1)
    val setone = Set(
      (List("言","13","55","43"), SortingCodes.FourCode),
      (List("言","13","55","34"), SortingCodes.FourCode))
    val settwo = Set(
      (List("41","11","25","11","35","43"), SortingCodes.SixCode), //4111251135543
      (List("41","11","25","11","35","34"), SortingCodes.SixCode), //4111251135534
      (List("11","11","25","11","35","43"), SortingCodes.SixCode), //1111251135543
      (List("11","11","25","11","35","34"), SortingCodes.SixCode)) //1111251135534

    seudo1 shouldBe setone ++ settwo
  }


  it should "test that element char generates correct seudo code" in {
    val singleElemInp = Set(
      (List("言", ""), 4),
      (List("1111251"), 6),
      (List("4111251"),6))
  
    val seudo2: Set[(List[String], AsortingCriteria)] =
      AgenerateSeudoCodes.convertElemAndRemainderToSeudo(singleElemInp)
    val output2 = Set(
      (List("言"), SortingCodes.OneCode),
      (List("言", fil, fil, fil), SortingCodes.FourCode),
      (List("11", "11", "25", "1", fil, fil), SortingCodes.SixCode),
      (List("41", "11", "25", "1", fil, fil), SortingCodes.SixCode))
    seudo2 shouldBe output2
  }
}






