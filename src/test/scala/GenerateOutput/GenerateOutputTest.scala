package GenerateOutput

import OutputTranslation.OutputSorting
import UtilityClasses.OutputEntry
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.SortedMap
import scala.util.{Failure, Success}

class GenerateOutputTest extends AnyFlatSpec with Matchers {


  it should "test output junda file" in {

    val generate = new GenerateOutputStrings()
    val outputLines: List[String] = generate.generateWithSpecial(GenerateOutputStrings.mapFullJunda)
    //write file
    var writesuccess: Boolean = false
/*
    generate.writeListToFile(outputLines, "outjunda2.txt", "src/test/scala/GenerateOutput") match {
      case Success(_) => writesuccess = true
      case Failure(e) => writesuccess = false
    }
    writesuccess shouldBe true*/
  }

  it should "test output junda file with meaning" in {

    val generate = new GenerateOutputStrings()
    val outputLines: List[String] = generate.generateWithMeaning(GenerateOutputStrings.mapFullJunda)
    //write file
    var writesuccess: Boolean = false
/*
    generate.writeListToFile(outputLines, "outjundaMeaning2.txt", "src/test/scala/GenerateOutput") match {
      case Success(_) => writesuccess = true
      case Failure(e) => writesuccess = false
    }
    writesuccess shouldBe true*/
  }
}
