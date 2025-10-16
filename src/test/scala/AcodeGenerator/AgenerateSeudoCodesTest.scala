package AcodeGenerator

import AcodeGenerators.AgenerateSeudoCodes
import Adatasources.ManualData.AcodelengthRules
import Atypes.{AsortingCriteria, PossibleWordCodes, SortingCodes}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class AgenerateSeudoCodesTest extends AnyFlatSpec with Matchers {

  val codeMap: Map[String, String] = AcodelengthRules.elementTypes
  val fil: String = AcodelengthRules.fil

  it should "test that multi word helper can generate code - characters with many codes" in {
    val input1 = (List("言", "135543"), PossibleWordCodes.FirstCode)
    val seudo1: List[String] = AgenerateSeudoCodes.splitCodeListHelperMultiChar(List(), input1) //reslist: List[String], inp: List[String]
    seudo1 shouldBe List("言")

    val input1b = (List("言", "135543"), PossibleWordCodes.FirstLastCode)
    val seudo1b: List[String] = AgenerateSeudoCodes.splitCodeListHelperMultiChar(List(), input1b) //reslist: List[String], inp: List[String]
    seudo1b shouldBe List("言", "43")

    val input1c = (List("言", "135543"), PossibleWordCodes.FirstSecondLastCode)
    val seudo1c: List[String] = AgenerateSeudoCodes.splitCodeListHelperMultiChar(List(), input1c) //reslist: List[String], inp: List[String]
    seudo1c shouldBe List("言", "13", "43")

    val input2a = (List("4111252"), PossibleWordCodes.FirstCode)
    val seudo2a: List[String] = AgenerateSeudoCodes.splitCodeListHelperMultiChar(List(), input2a) //reslist: List[String], inp: List[String]
    seudo2a shouldBe List("41")

    val input2b = (List("4111252"), PossibleWordCodes.FirstLastCode)
    val seudo2b: List[String] = AgenerateSeudoCodes.splitCodeListHelperMultiChar(List(), input2b) //reslist: List[String], inp: List[String]
    seudo2b shouldBe List("41", "52")

    val input2c = (List("4111252"), PossibleWordCodes.FirstSecondLastCode)
    val seudo2c: List[String] = AgenerateSeudoCodes.splitCodeListHelperMultiChar(List(), input2c) //reslist: List[String], inp: List[String]
    seudo2c shouldBe List("41", "11", "52")

  }


  it should "test that multi word helper can generate code - characters with few codes" in {
    val input1 = (List("言", ""), PossibleWordCodes.FirstCode)
    val seudo1: List[String] = AgenerateSeudoCodes.splitCodeListHelperMultiChar(List(), input1) //reslist: List[String], inp: List[String]
    seudo1 shouldBe List("言")

    val input1b = (List("言", ""), PossibleWordCodes.FirstCode)
    val seudo1b: List[String] = AgenerateSeudoCodes.splitCodeListHelperMultiChar(List(), input1b) //reslist: List[String], inp: List[String]
    seudo1b shouldBe List("言")

    val input1c = (List("言", ""), PossibleWordCodes.FirstSecondLastCode)
    val seudo1c: List[String] = AgenerateSeudoCodes.splitCodeListHelperMultiChar(List(), input1c) //reslist: List[String], inp: List[String]
    seudo1c shouldBe List("言")

    val input2a = (List("4"), PossibleWordCodes.FirstCode)
    val seudo2a: List[String] = AgenerateSeudoCodes.splitCodeListHelperMultiChar(List(), input2a) //reslist: List[String], inp: List[String]
    seudo2a shouldBe List("4")

    val input2b = (List("4"), PossibleWordCodes.FirstLastCode)
    val seudo2b: List[String] = AgenerateSeudoCodes.splitCodeListHelperMultiChar(List(), input2b) //reslist: List[String], inp: List[String]
    seudo2b shouldBe List("4")

    val input2c = (List("4"), PossibleWordCodes.FirstSecondLastCode)
    val seudo2c: List[String] = AgenerateSeudoCodes.splitCodeListHelperMultiChar(List(), input2c) //reslist: List[String], inp: List[String]
    seudo2c shouldBe List("4")

    val input3a = (List("413"), PossibleWordCodes.FirstCode)
    val seudo3a: List[String] = AgenerateSeudoCodes.splitCodeListHelperMultiChar(List(), input3a) //reslist: List[String], inp: List[String]
    seudo3a shouldBe List("41")

    val input3b = (List("413"), PossibleWordCodes.FirstLastCode)
    val seudo3b: List[String] = AgenerateSeudoCodes.splitCodeListHelperMultiChar(List(), input3b) //reslist: List[String], inp: List[String]
    seudo3b shouldBe List("41", "3")

    val input3c = (List("413"), PossibleWordCodes.FirstSecondLastCode)
    val seudo3c: List[String] = AgenerateSeudoCodes.splitCodeListHelperMultiChar(List(), input3c) //reslist: List[String], inp: List[String]
    seudo3c shouldBe List("41", "3")

  }




  it should "test that seudo letters can be generated from chars" in {
    val input1 = Set(
      (List("言", "135543"),4),
      (List("言", "135534"),4),
      (List("4111251135543"),6),
      (List("4111251135534"),6),
      (List("1111251135543"),6),
      (List("1111251135534"),6),
    )
    val seudo1: Set[(List[String], AsortingCriteria)] = AgenerateSeudoCodes.convertElemAndRemainderToSeudoSingleChar(input1)
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
      AgenerateSeudoCodes.convertElemAndRemainderToSeudoSingleChar(singleElemInp)
    val output2 = Set(
      (List("言"), SortingCodes.OneCode),
      (List("言", fil, fil, fil), SortingCodes.FourCode),
      (List("11", "11", "25", "1", fil, fil), SortingCodes.SixCode),
      (List("41", "11", "25", "1", fil, fil), SortingCodes.SixCode))
    seudo2 shouldBe output2
  }
}






