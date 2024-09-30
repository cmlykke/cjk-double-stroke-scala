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
    val readMeta = new ReadConfigFiles()
    val dictSimp: List[String] = readMeta.readConfig("simpDictConfig_basic.txt")
    val outputLines: List[String] = generate.generateWithSpecial(GenerateOutputStrings.mapFullJunda)
    val mergedList: List[String] = dictSimp ++ outputLines
    //write file
    var writesuccess: Boolean = false
/*
    generate.writeListToFile(mergedList, "POFsimp.dict.yaml", "src/test/scala/GenerateOutput") match {
      case Success(_) => writesuccess = true
      case Failure(e) => writesuccess = false
    }
    writesuccess shouldBe true*/
  }

  it should "test output junda file - schema" in {
    val readMeta = new ReadConfigFiles()
    val dictSimp: List[String] = readMeta.readConfig("simpDictConfig_basic.txt")
    val mergedList: List[String] = dictSimp
    /*
        generate.writeListToFile(mergedList, "POFsimp.schema.yaml", "src/test/scala/GenerateOutput") match {
          case Success(_) => writesuccess = true
          case Failure(e) => writesuccess = false
        }
        writesuccess shouldBe true*/
  }

  it should "test output junda file with meaning" in {

    val generate = new GenerateOutputStrings()
    val readMeta = new ReadConfigFiles()
    val dictSimp: List[String] = readMeta.readConfig("simpDictConfig_meaning.txt")
    val outputLines: List[String] = generate.generateWithMeaning(GenerateOutputStrings.mapFullJunda)
    val mergedList: List[String] = dictSimp ++ outputLines
    //write file
    var writesuccess: Boolean = false
/*
    generate.writeListToFile(mergedList, "POFsimpM.dict.yaml", "src/test/scala/GenerateOutput") match {
      case Success(_) => writesuccess = true
      case Failure(e) => writesuccess = false
    }
    writesuccess shouldBe true*/
  }

  it should "test output junda file with meaning - schema" in {
    val readMeta = new ReadConfigFiles()
    val dictSimp: List[String] = readMeta.readConfig("simpDictConfig_meaning.txt")
    val mergedList: List[String] = dictSimp
    /*
    generate.writeListToFile(mergedList, "POFsimpM.schema.yaml", "src/test/scala/GenerateOutput") match {
      case Success(_) => writesuccess = true
      case Failure(e) => writesuccess = false
    }
    writesuccess shouldBe true*/
  }
}
