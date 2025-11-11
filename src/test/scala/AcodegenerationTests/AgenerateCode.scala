package AcodegenerationTests

import ApublishingCodes.{AgenerateDocumentsForPublishing, AgenerateOutputStrings, AreadConfigFiles}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class AgenerateCode extends AnyFlatSpec with Matchers {

  val simpList: List[String] = AgenerateDocumentsForPublishing.generateOutputSimplifiedString()
  val tradList: List[String] = AgenerateDocumentsForPublishing.generateOutputTraditionalString()


  /*

  it should "test output default.custom.yaml file" in {

    val generate = new AgenerateOutputStrings()
    val readMeta = new AreadConfigFiles()
    val dictSimp: List[String] = readMeta.readConfig("defaultcustom.txt")
    //val outputLines: List[String] = generate.generateWithSpecial(GenerateOutputStrings.mapFullJunda)
    val mergedList: List[String] = dictSimp //++ outputLines
    //write file
    var writesuccess: Boolean = false

    generate.writeListToFile(mergedList, "default.custom.yaml", "src/test/scala/AcodegenerationTests")
    /*match {
      case Success(_) => writesuccess = true
      case Failure(e) => writesuccess = false
    }
    writesuccess shouldBe true*/
  }

  it should "test output junda file" in {

    val generate = new AgenerateOutputStrings()
    val readMeta = new AreadConfigFiles()
    val dictSimp: List[String] = readMeta.readConfig("simpDictConfig_basic.txt")
    val outputLines: List[String] = simpList
    val mergedList: List[String] = dictSimp ++ outputLines
    //write file
    var writesuccess: Boolean = false

    generate.writeListToFile(mergedList, "liumajian.dict.yaml", "src/test/scala/AcodegenerationTests")
    /*match {
      case Success(_) => writesuccess = true
      case Failure(e) => writesuccess = false
    }
    writesuccess shouldBe true*/
  }

  it should "test output junda file - schema" in {

    val generate = new AgenerateOutputStrings()
    val readMeta = new AreadConfigFiles()
    //val dictSimp: List[String] = readMeta.readConfig("simpDictConfig_basic.txt")
    val dictSimp: List[String] = readMeta.readConfig("simpSchemaConfig_basic.txt")
    val mergedList: List[String] = dictSimp
    var writesuccess: Boolean = false

    generate.writeListToFile(mergedList, "liumajian.schema.yaml", "src/test/scala/AcodegenerationTests")
    /*match {
      case Success(_) => writesuccess = true
      case Failure(e) => writesuccess = false
    }
    writesuccess shouldBe true*/
  }

  it should "test output tzai file" in {

    val generate = new AgenerateOutputStrings()
    val readMeta = new AreadConfigFiles()
    val dictTrad: List[String] = readMeta.readConfig("tradDictConfig_basic.txt")
    val outputLines: List[String] = tradList
    val mergedList: List[String] = dictTrad ++ outputLines
    //write file
    var writesuccess: Boolean = false

    generate.writeListToFile(mergedList, "liumafan.dict.yaml", "src/test/scala/AcodegenerationTests")
    /*match {
          case Success(_) => writesuccess = true
          case Failure(e) => writesuccess = false
        }
        writesuccess shouldBe true*/
  }

  it should "test output tzai file - schema" in {

    val generate = new AgenerateOutputStrings()
    val readMeta = new AreadConfigFiles()
    //val dictTrad: List[String] = readMeta.readConfig("tradDictConfig_basic.txt")
    val dictTrad: List[String] = readMeta.readConfig("tradSchemaConfig_basic.txt")
    val mergedList: List[String] = dictTrad
    var writesuccess: Boolean = false

    generate.writeListToFile(mergedList, "liumafan.schema.yaml", "src/test/scala/AcodegenerationTests")
    /*match {
          case Success(_) => writesuccess = true
          case Failure(e) => writesuccess = false
        }
        writesuccess shouldBe true*/
  }

   */
}
