package AcodeGenerator

import AcodeGenerators.{AgenerateElemAndRemainderLists, AgenerateFinalSeudoCodes}
import Adatasources.FileReaders.{AidsData, AreadConwayData}
import Adatasources.ManualData.Aelements
import Atypes.{Aelementstype, Agrapheme, AsortingCriteria, SortingCodes}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class AgenerateFinalSeudoCodesTest extends AnyFlatSpec with Matchers {

  val conwaymap = AreadConwayData.mapConwayData()
  val idsmap = AidsData.idsDataRaw()
  val idsToStrokeMap: Map[String, Aelementstype] = Aelements.idsToStrokeMap


  it should "test that seudo code can be geerated from two characters" in {

    val res2: Set[List[String]] =
      AgenerateFinalSeudoCodes.seudo5codesFromFullWords(
        List(Agrapheme("摳"), Agrapheme("腳")), conwaymap, idsmap, idsToStrokeMap)
    res2.shouldEqual(
      Set(List("手","15", "35", "11", "52"), List("手","15", "35", "41", "52")))
  }

  it should "test that seudo code can be geerated from six characters" in {
    //
    val res2: Set[List[String]] =
      AgenerateFinalSeudoCodes.seudo5codesFromFullWords(
        List(
          Agrapheme("中"),
          Agrapheme("华"),
          Agrapheme("人"),
          Agrapheme("民"),
          Agrapheme("共"),
          Agrapheme("和"),
          Agrapheme("国")
        ), conwaymap, idsmap, idsToStrokeMap)
    res2.shouldEqual(
      Set(List("25", "32", "34", "51", "12")))
  }

  it should "test that seudo code can be geerated from single characters" in {

    val res2:  Set[(List[String], AsortingCriteria)] =
      AgenerateFinalSeudoCodes.seudoCodesFromSingleChar(Agrapheme("誠"), conwaymap, idsmap, idsToStrokeMap)
    res2.shouldEqual(
      Set((List("言", "13", "55", "43"), SortingCodes.FourCode),
        (List("言", "13", "55", "34"), SortingCodes.FourCode),
        (List("41", "11", "25", "11", "35", "43"), SortingCodes.SixCode),
        (List("41", "11", "25", "11", "35", "34"), SortingCodes.SixCode),
        (List("11", "11", "25", "11", "35", "43"), SortingCodes.SixCode),
        (List("11", "11", "25", "11", "35", "34"), SortingCodes.SixCode)
      ))
  }

}
