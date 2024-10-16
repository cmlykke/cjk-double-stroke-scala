package GenerateOutput

import OutputTranslation.OutputSorting
import UtilityClasses.OutputEntry
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.SortedMap
import scala.util.{Failure, Success}

class GenerateOutputTest extends AnyFlatSpec with Matchers {


  it should "test output default.custom.yaml file" in {

    val generate = new GenerateOutputStrings()
    val readMeta = new ReadConfigFiles()
    val dictSimp: List[String] = readMeta.readConfig("defaultcustom.txt")
    //val outputLines: List[String] = generate.generateWithSpecial(GenerateOutputStrings.mapFullJunda)
    val mergedList: List[String] = dictSimp //++ outputLines
    //write file
    var writesuccess: Boolean = false

    generate.writeListToFile(mergedList, "default.custom.yaml", "src/test/scala/GenerateOutput") match {
      case Success(_) => writesuccess = true
      case Failure(e) => writesuccess = false
    }
    writesuccess shouldBe true
  }
  
  it should "test output junda file" in {

    val generate = new GenerateOutputStrings()
    val readMeta = new ReadConfigFiles()
    val dictSimp: List[String] = readMeta.readConfig("simpDictConfig_basic.txt")
    val outputLines: List[String] = generate.generateWithSpecial(GenerateOutputStrings.mapFullJunda)
    val mergedList: List[String] = dictSimp ++ outputLines
    //write file
    var writesuccess: Boolean = false

    generate.writeListToFile(mergedList, "liumajian.dict.yaml", "src/test/scala/GenerateOutput") match {
      case Success(_) => writesuccess = true
      case Failure(e) => writesuccess = false
    }
    writesuccess shouldBe true
  }

  it should "test output junda file - schema" in {

    val generate = new GenerateOutputStrings()
    val readMeta = new ReadConfigFiles()
    //val dictSimp: List[String] = readMeta.readConfig("simpDictConfig_basic.txt")
    val dictSimp: List[String] = readMeta.readConfig("simpSchemaConfig_basic.txt")
    val mergedList: List[String] = dictSimp
    var writesuccess: Boolean = false

        generate.writeListToFile(mergedList, "liumajian.schema.yaml", "src/test/scala/GenerateOutput") match {
          case Success(_) => writesuccess = true
          case Failure(e) => writesuccess = false
        }
        writesuccess shouldBe true
  }

  it should "test output tzai file" in {

    val generate = new GenerateOutputStrings()
    val readMeta = new ReadConfigFiles()
    val dictTrad: List[String] = readMeta.readConfig("tradDictConfig_basic.txt")
    val outputLines: List[String] = generate.generateWithSpecial(GenerateOutputStrings.mapFullTzai)
    val mergedList: List[String] = dictTrad ++ outputLines
    //write file
    var writesuccess: Boolean = false

        generate.writeListToFile(mergedList, "liumafan.dict.yaml", "src/test/scala/GenerateOutput") match {
          case Success(_) => writesuccess = true
          case Failure(e) => writesuccess = false
        }
        writesuccess shouldBe true
  }

  it should "test output tzai file - schema" in {

    val generate = new GenerateOutputStrings()
    val readMeta = new ReadConfigFiles()
    //val dictTrad: List[String] = readMeta.readConfig("tradDictConfig_basic.txt")
    val dictTrad: List[String] = readMeta.readConfig("tradSchemaConfig_basic.txt")
    val mergedList: List[String] = dictTrad
    var writesuccess: Boolean = false

        generate.writeListToFile(mergedList, "liumafan.schema.yaml", "src/test/scala/GenerateOutput") match {
          case Success(_) => writesuccess = true
          case Failure(e) => writesuccess = false
        }
        writesuccess shouldBe true
  }

 
}
