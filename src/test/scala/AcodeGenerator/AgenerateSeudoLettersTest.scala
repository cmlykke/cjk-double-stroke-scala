package AcodeGenerator

import AcodeGenerators.generateSeudoLetters
import Adatasources.FileReaders.{AidsData, AreadConwayData}
import AgraphemeToCodeConverters.AgraphemeToStrokeSet
import Atypes.{AconwayColl, Agrapheme}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.collection.mutable
import scala.collection.immutable

import scala.collection.immutable

class AgenerateSeudoLettersTest extends AnyFlatSpec with Matchers {

  val conwaymap = AreadConwayData.mapConwayData()
  val idsmap = AidsData.idsDataRaw()


  it should "test that seudo letters can be generated from chars" in {

    val res = generateSeudoLetters.getsplitcodesfromchar(Agrapheme("匕"), conwaymap, idsmap)
    val test = ""

  }

}
