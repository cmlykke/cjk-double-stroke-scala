package AcodegenerationTests

import ApublishingCodes.{AgenerateOutputStrings, AreadConfigFiles}
import Asingletons.AsingletonsForTests
import Atypes.AsortingObject
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class AgenerateCode extends AnyFlatSpec with Matchers {

  private val outputSImp: List[(String, String, AsortingObject)] = AsingletonsForTests.outputSortedSimp
  private val outputTrad: List[(String, String, AsortingObject)] = AsingletonsForTests.outputSortedTrad
  private val specialCharacters: List[String] = AgenerateOutputStrings.generateSpecialCharacterStrList().toList

  /*

  it should "test output default.custom.yaml file" in {

    val readMeta = new AreadConfigFiles()
    val dictSimp: List[String] = readMeta.readConfig("defaultcustom.txt")
    //val outputLines: List[String] = generate.generateWithSpecial(GenerateOutputStrings.mapFullJunda)
    val mergedList: List[String] = dictSimp //++ outputLines
    //write file
    var writesuccess: Boolean = false

    AgenerateOutputStrings.writeListToFile(mergedList, "default.custom.yaml", "src/test/scala/AcodegenerationTests")
    /*match {
      case Success(_) => writesuccess = true
      case Failure(e) => writesuccess = false
    }
    writesuccess shouldBe true*/
  }

  it should "test output junda file" in {

    val readMeta = new AreadConfigFiles()
    val dictSimp: List[String] = readMeta.readConfig("simpDictConfig_basic.txt")
    val outputLines: List[String] = outputSImp.map(x => x._1 + "\t" + x._2)
    val mergedList: List[String] = dictSimp ++ specialCharacters ++ outputLines


    val kg_before = outputSImp.filter(x => x._2 == "kg")
    val kg_after = outputLines.filter(x => x.endsWith("kg"))
    val kg_last = mergedList.filter(x => x.endsWith("kg"))

    //write file
    var writesuccess: Boolean = false

    AgenerateOutputStrings.writeListToFile(mergedList, "liumajian.dict.yaml", "src/test/scala/AcodegenerationTests")
    /*match {
      case Success(_) => writesuccess = true
      case Failure(e) => writesuccess = false
    }
    writesuccess shouldBe true*/
  }

  it should "test output junda file - schema" in {

    val readMeta = new AreadConfigFiles()
    //val dictSimp: List[String] = readMeta.readConfig("simpDictConfig_basic.txt")
    val dictSimp: List[String] = readMeta.readConfig("simpSchemaConfig_basic.txt")
    val mergedList: List[String] = dictSimp
    var writesuccess: Boolean = false

    AgenerateOutputStrings.writeListToFile(mergedList, "liumajian.schema.yaml", "src/test/scala/AcodegenerationTests")
    /*match {
      case Success(_) => writesuccess = true
      case Failure(e) => writesuccess = false
    }
    writesuccess shouldBe true*/
  }

  it should "test output tzai file" in {

    val readMeta = new AreadConfigFiles()
    val dictTrad: List[String] = readMeta.readConfig("tradDictConfig_basic.txt")
    val outputLines: List[String] = outputTrad.map(x => x._1 + "\t" + x._2)
    val mergedList: List[String] = dictTrad ++ specialCharacters ++ outputLines
    //write file



    val kg_before = outputTrad.filter(x => x._2 == "kg")
    val kg_after = outputLines.filter(x => x.endsWith("kg"))
    val kg_last = mergedList.filter(x => x.endsWith("kg"))


    var writesuccess: Boolean = false

    AgenerateOutputStrings.writeListToFile(mergedList, "liumafan.dict.yaml", "src/test/scala/AcodegenerationTests")
    /*match {
          case Success(_) => writesuccess = true
          case Failure(e) => writesuccess = false
        }
        writesuccess shouldBe true*/
  }

  it should "test output tzai file - schema" in {

    val readMeta = new AreadConfigFiles()
    //val dictTrad: List[String] = readMeta.readConfig("tradDictConfig_basic.txt")
    val dictTrad: List[String] = readMeta.readConfig("tradSchemaConfig_basic.txt")
    val mergedList: List[String] = dictTrad
    var writesuccess: Boolean = false


    AgenerateOutputStrings.writeListToFile(mergedList, "liumafan.schema.yaml", "src/test/scala/AcodegenerationTests")
    /*match {
          case Success(_) => writesuccess = true
          case Failure(e) => writesuccess = false
        }
        writesuccess shouldBe true*/
  }
*/
}
