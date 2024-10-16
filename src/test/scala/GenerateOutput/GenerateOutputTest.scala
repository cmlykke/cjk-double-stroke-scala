package GenerateOutput

import OutputTranslation.OutputSorting
import UtilityClasses.OutputEntry
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.SortedMap
import scala.util.{Failure, Success}

class GenerateOutputTest extends AnyFlatSpec with Matchers {

/*

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

    generate.writeListToFile(mergedList, "POFsimp.dict.yaml", "src/test/scala/GenerateOutput") match {
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

        generate.writeListToFile(mergedList, "POFsimp.schema.yaml", "src/test/scala/GenerateOutput") match {
          case Success(_) => writesuccess = true
          case Failure(e) => writesuccess = false
        }
        writesuccess shouldBe true
  }

  it should "test output junda file with meaning" in {

    val generate = new GenerateOutputStrings()
    val readMeta = new ReadConfigFiles()
    val dictSimp: List[String] = readMeta.readConfig("simpDictConfig_meaning.txt")
    val outputLines: List[String] = generate.generateWithMeaning(GenerateOutputStrings.mapFullJunda)
    val mergedList: List[String] = dictSimp ++ outputLines
    //write file
    var writesuccess: Boolean = false

    generate.writeListToFile(mergedList, "POFsimpM.dict.yaml", "src/test/scala/GenerateOutput") match {
      case Success(_) => writesuccess = true
      case Failure(e) => writesuccess = false
    }
    writesuccess shouldBe true
  }

  it should "test output junda file with meaning - schema" in {

    val generate = new GenerateOutputStrings()
    val readMeta = new ReadConfigFiles()
    val dictSimp: List[String] = readMeta.readConfig("simpSchemaConfig_meaning.txt")
    val mergedList: List[String] = dictSimp
    var writesuccess: Boolean = false

    generate.writeListToFile(mergedList, "POFsimpM.schema.yaml", "src/test/scala/GenerateOutput") match {
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

        generate.writeListToFile(mergedList, "POFtrad.dict.yaml", "src/test/scala/GenerateOutput") match {
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

        generate.writeListToFile(mergedList, "POFtrad.schema.yaml", "src/test/scala/GenerateOutput") match {
          case Success(_) => writesuccess = true
          case Failure(e) => writesuccess = false
        }
        writesuccess shouldBe true
  }

  it should "test output tzai file with meaning" in {

    val generate = new GenerateOutputStrings()
    val readMeta = new ReadConfigFiles()
    val dictTrad: List[String] = readMeta.readConfig("tradDictConfig_meaning.txt")
    val outputLines: List[String] = generate.generateWithMeaning(GenerateOutputStrings.mapFullTzai)
    val mergedList: List[String] = dictTrad ++ outputLines
    //write file
    var writesuccess: Boolean = false

        generate.writeListToFile(mergedList, "POFtradM.dict.yaml", "src/test/scala/GenerateOutput") match {
          case Success(_) => writesuccess = true
          case Failure(e) => writesuccess = false
        }
        writesuccess shouldBe true
  }

  it should "test output tzai file with meaning - schema" in {

    val generate = new GenerateOutputStrings()
    val readMeta = new ReadConfigFiles()
    val dictTrad: List[String] = readMeta.readConfig("tradSchemaConfig_meaning.txt")
    val mergedList: List[String] = dictTrad
    var writesuccess: Boolean = false

    generate.writeListToFile(mergedList, "POFtradM.schema.yaml", "src/test/scala/GenerateOutput") match {
      case Success(_) => writesuccess = true
      case Failure(e) => writesuccess = false
    }
    writesuccess shouldBe true
  }
 
 */
}
