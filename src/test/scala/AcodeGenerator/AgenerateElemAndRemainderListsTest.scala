package AcodeGenerator

import AcodeGenerators.AgenerateElemAndRemainderLists
import Adatasources.FileReaders.{AidsData, AreadConwayData}
import Adatasources.ManualData.Aelements
import AgraphemeToCodeConverters.AgraphemeToStrokeSet
import Asingletons.AsingletonsForTests
import Atypes.{AconwayColl, Aelementstype, Agrapheme}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.mutable
import scala.collection.immutable
import scala.collection.immutable

class AgenerateElemAndRemainderListsTest extends AnyFlatSpec with Matchers {

  it should "test that seudo letters can be generated from chars" in {

    val resFour = AgenerateElemAndRemainderLists.getsplitFourCodesfromchar(
      Agrapheme("誠"), AsingletonsForTests.conwaymap, AsingletonsForTests.idsmap, AsingletonsForTests.idsToStrokeMap)
    resFour.shouldEqual(
      Set((List("言", "135543"), 4),
        (List("言", "135534"), 4)
      ))
    val resSix = AgenerateElemAndRemainderLists.getsplitSixCodesfromchar(
      Agrapheme("誠"),AsingletonsForTests.conwaymap, AsingletonsForTests.idsmap, AsingletonsForTests.idsToStrokeMap)
    resSix.shouldEqual(
      Set(
        (List("4111251135543"), 6),
        (List("4111251135534"), 6),
        (List("1111251135543"), 6),
        (List("1111251135534"), 6),
      ))
  }

  it should "test that seudo letters can be generated from element" in {
    val resFour = AgenerateElemAndRemainderLists.getsplitFourCodesfromchar(
      Agrapheme("言"),AsingletonsForTests.conwaymap, AsingletonsForTests.idsmap, AsingletonsForTests.idsToStrokeMap)
    resFour.shouldEqual(
      Set(
        (List("言", ""), 4)))
    val resSix = AgenerateElemAndRemainderLists.getsplitSixCodesfromchar(
      Agrapheme("言"),AsingletonsForTests.conwaymap, AsingletonsForTests.idsmap, AsingletonsForTests.idsToStrokeMap)
    resSix.shouldEqual(
      Set(
        (List("1111251"), 6),
        (List("4111251"), 6)))
  }

}
