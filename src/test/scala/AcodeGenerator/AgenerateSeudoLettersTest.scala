package AcodeGenerator

import AcodeGenerators.AgenerateSeudoLetters
import Adatasources.FileReaders.{AidsData, AreadConwayData}
import AgraphemeToCodeConverters.AgraphemeToStrokeSet
import Atypes.{AconwayColl, Aelements, Aelementstype, Agrapheme}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.mutable
import scala.collection.immutable
import scala.collection.immutable

class AgenerateSeudoLettersTest extends AnyFlatSpec with Matchers {

  val conwaymap = AreadConwayData.mapConwayData()
  val idsmap = AidsData.idsDataRaw()
  val idsToStrokeMap: Map[String, Aelementstype] = Aelements.idsToStrokeMap

  it should "test that seudo letters can be generated from chars" in {

    val res2 = AgenerateSeudoLetters.getsplitcodesfromchar(Agrapheme("誠"), conwaymap, idsmap, idsToStrokeMap)
    res2.shouldEqual(Set(List("言", "135543"), List("言", "135534")))

  }

}
