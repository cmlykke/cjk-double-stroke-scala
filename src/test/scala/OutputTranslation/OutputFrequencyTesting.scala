package OutputTranslation

import UtilityClasses.CharSystem.{Junda, Tzai}
import UtilityClasses.{CedictEntry, CharSystem, OutputEntry}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import staticFileGenerators.cedictMap.GenerateCedictMap

import scala.collection.immutable.SortedMap
import scala.collection.mutable.ListBuffer

class OutputFrequencyTesting extends AnyFlatSpec with Matchers {

  val singleOut: Set[OutputEntry] = OutputTranslation.jundaSingelOut
  val multiOut: Set[OutputEntry] = OutputTranslation.jundaMultiOut
  val singleOutTzai: Set[OutputEntry] = OutputTranslation.tzaiSingelOut
  val multiOutTzai: Set[OutputEntry] = OutputTranslation.tzaiMultiOut

  it should "check the number of uses of left vs right hand - single characters" in {

    val jundaUnchanged: (Double, Double) = countLeftRight(singleOut, Map(), Junda)
    val jundaUnchangedLeftSelection: (Double, Double) = countLeftRightWithSelection(singleOut, Map(), Junda)
    val jundaTopChanged: (Double, Double) = countLeftRight(singleOut, changeTopRow(), Junda)
    val jundaTopChangedLeftSelection: (Double, Double) = countLeftRightWithSelection(singleOut, changeTopRow(), Junda)

    val jundaMiddleChanged: (Double, Double) = countLeftRight(singleOut, changeMiddleRow(), Junda)
    val jundaMiddleChangedWithSelection: (Double, Double) = countLeftRightWithSelection(singleOut, changeMiddleRow(), Junda)
    val jundaButtomChanged: (Double, Double) = countLeftRight(singleOut, changeButtomRow(), Junda)
    val jundaMiddleAndButtonChanged: (Double, Double) = countLeftRight(singleOut, changeMiddleAndButtomRow(), Junda)

    val tzaiUnchanged: (Double, Double) = countLeftRight(singleOutTzai, Map(), Tzai)
    val tzaiTopChanged: (Double, Double) = countLeftRight(singleOutTzai, changeTopRow(), Tzai)
    val tzaiMiddleChanged: (Double, Double) = countLeftRight(singleOutTzai, changeMiddleRow(), Tzai)
    val tzaiButtomChanged: (Double, Double) = countLeftRight(singleOutTzai, changeButtomRow(), Tzai)
    val tzaiMiddleAndButtonChanged: (Double, Double) = countLeftRight(singleOutTzai, changeMiddleAndButtomRow(), Tzai)

    //junda
    var strResult = ""
    + approximatelyEqual("jundaUnchanged._1", jundaUnchanged._1)
    + approximatelyEqual("jundaUnchanged._2", jundaUnchanged._2)
    + approximatelyEqual("jundaUnchangedLeftSelection._1", jundaUnchangedLeftSelection._1)
    + approximatelyEqual("jundaTopChanged._1", jundaTopChanged._1)
    + approximatelyEqual("jundaTopChanged._2", jundaTopChanged._2)
    + approximatelyEqual("jundaTopChangedLeftSelection._1", jundaTopChangedLeftSelection._1)
    //jundaTopChangedLeftSelection
    + approximatelyEqual("jundaMiddleChanged._1", jundaMiddleChanged._1)
    + approximatelyEqual("jundaMiddleChanged._2", jundaMiddleChanged._2)
    + approximatelyEqual("jundaMiddleChangedWithSelection._1", jundaMiddleChangedWithSelection._1)
    + approximatelyEqual("jundaButtomChanged._1", jundaButtomChanged._1)
    + approximatelyEqual("jundaButtomChanged._2", jundaButtomChanged._2)
    + approximatelyEqual("jundaMiddleAndButtonChanged._1", jundaMiddleAndButtonChanged._1)
    + approximatelyEqual("jundaMiddleAndButtonChanged._2", jundaMiddleAndButtonChanged._2)
    + approximatelyEqual("tzaiUnchanged._1", tzaiUnchanged._1)
    + approximatelyEqual("tzaiUnchanged._2", tzaiUnchanged._2)
    + approximatelyEqual("tzaiTopChanged._1", tzaiTopChanged._1)
    + approximatelyEqual("tzaiTopChanged._2", tzaiTopChanged._2)
    + approximatelyEqual("tzaiMiddleChanged._1", tzaiMiddleChanged._1)
    + approximatelyEqual("tzaiMiddleChanged._2", tzaiMiddleChanged._2)
    + approximatelyEqual("tzaiButtomChanged._1", tzaiButtomChanged._1)
    + approximatelyEqual("tzaiButtomChanged._2", tzaiButtomChanged._2)
    + approximatelyEqual("tzaiMiddleAndButtonChanged._1", tzaiMiddleAndButtonChanged._1)
    + approximatelyEqual("tzaiMiddleAndButtonChanged._2", tzaiMiddleAndButtonChanged._2)

    val finalRes = strResult.replaceAll("\\s", "")
    finalRes shouldBe """3.313 jundaUnchanged._1
                        |4.297 jundaUnchanged._2
                        |5.659 jundaUnchangedLeftSelection._1
                        |3.712 jundaTopChanged._1
                        |3.898 jundaTopChanged._2
                        |6.058 jundaTopChangedLeftSelection._1
                        |4.139 jundaMiddleChanged._1
                        |3.471 jundaMiddleChanged._2
                        |6.485 jundaMiddleChangedWithSelection._1
                        |3.384 jundaButtomChanged._1
                        |4.226 jundaButtomChanged._2
                        |4.210 jundaMiddleAndButtonChanged._1
                        |3.400 jundaMiddleAndButtonChanged._2
                        |3.240 tzaiUnchanged._1
                        |4.724 tzaiUnchanged._2
                        |3.958 tzaiTopChanged._1
                        |4.005 tzaiTopChanged._2
                        |4.315 tzaiMiddleChanged._1
                        |3.648 tzaiMiddleChanged._2
                        |3.225 tzaiButtomChanged._1
                        |4.738 tzaiButtomChanged._2
                        |4.300 tzaiMiddleAndButtonChanged._1
                        |3.663 tzaiMiddleAndButtonChanged._2
                        |""".stripMargin.replaceAll("\\s", "")
  }


  it should "check the number of uses of left vs right hand - multi character words" in {

    val jundaUnchanged: (Double, Double) = countLeftRight(multiOut, Map(), Junda)
    val jundaTopChanged: (Double, Double) = countLeftRight(multiOut, changeTopRow(), Junda)
    val jundaMiddleChanged: (Double, Double) = countLeftRight(multiOut, changeMiddleRow(), Junda)
    val jundaButtomChanged: (Double, Double) = countLeftRight(multiOut, changeButtomRow(), Junda)
    val jundaMiddleAndButtonChanged: (Double, Double) = countLeftRight(multiOut, changeMiddleAndButtomRow(), Junda)

    val tzaiUnchanged: (Double, Double) = countLeftRight(multiOutTzai, Map(), Tzai)
    val tzaiTopChanged: (Double, Double) = countLeftRight(multiOutTzai, changeTopRow(), Tzai)
    val tzaiMiddleChanged: (Double, Double) = countLeftRight(multiOutTzai, changeMiddleRow(), Tzai)
    val tzaiButtomChanged: (Double, Double) = countLeftRight(multiOutTzai, changeButtomRow(), Tzai)
    val tzaiMiddleAndButtonChanged: (Double, Double) = countLeftRight(multiOutTzai, changeMiddleAndButtomRow(), Tzai)

    //junda
    var strResult = ""
    + approximatelyEqual("jundaUnchanged._1", jundaUnchanged._1)
      + approximatelyEqual("jundaUnchanged._2", jundaUnchanged._2)
      + approximatelyEqual("jundaTopChanged._1", jundaTopChanged._1)
      + approximatelyEqual("jundaTopChanged._2", jundaTopChanged._2)
      + approximatelyEqual("jundaMiddleChanged._1", jundaMiddleChanged._1)
      + approximatelyEqual("jundaMiddleChanged._2", jundaMiddleChanged._2)
      + approximatelyEqual("jundaButtomChanged._1", jundaButtomChanged._1)
      + approximatelyEqual("jundaButtomChanged._2", jundaButtomChanged._2)
      + approximatelyEqual("jundaMiddleAndButtonChanged._1", jundaMiddleAndButtonChanged._1)
      + approximatelyEqual("jundaMiddleAndButtonChanged._2", jundaMiddleAndButtonChanged._2)
      + approximatelyEqual("tzaiUnchanged._1", tzaiUnchanged._1)
      + approximatelyEqual("tzaiUnchanged._2", tzaiUnchanged._2)
      + approximatelyEqual("tzaiTopChanged._1", tzaiTopChanged._1)
      + approximatelyEqual("tzaiTopChanged._2", tzaiTopChanged._2)
      + approximatelyEqual("tzaiMiddleChanged._1", tzaiMiddleChanged._1)
      + approximatelyEqual("tzaiMiddleChanged._2", tzaiMiddleChanged._2)
      + approximatelyEqual("tzaiButtomChanged._1", tzaiButtomChanged._1)
      + approximatelyEqual("tzaiButtomChanged._2", tzaiButtomChanged._2)
      + approximatelyEqual("tzaiMiddleAndButtonChanged._1", tzaiMiddleAndButtonChanged._1)
      + approximatelyEqual("tzaiMiddleAndButtonChanged._2", tzaiMiddleAndButtonChanged._2)

    val finalRes = strResult.replaceAll("\\s", "")
    finalRes shouldBe """48.84 jundaUnchanged._1
                        |62.42 jundaUnchanged._2
                        |52.47 jundaTopChanged._1
                        |58.79 jundaTopChanged._2
                        |61.77 jundaMiddleChanged._1
                        |49.48 jundaMiddleChanged._2
                        |49.83 jundaButtomChanged._1
                        |61.43 jundaButtomChanged._2
                        |62.76 jundaMiddleAndButtonChanged._1
                        |48.49 jundaMiddleAndButtonChanged._2
                        |41.28 tzaiUnchanged._1
                        |62.73 tzaiUnchanged._2
                        |51.29 tzaiTopChanged._1
                        |52.72 tzaiTopChanged._2
                        |55.38 tzaiMiddleChanged._1
                        |48.63 tzaiMiddleChanged._2
                        |42.24 tzaiButtomChanged._1
                        |61.77 tzaiButtomChanged._2
                        |56.34 tzaiMiddleAndButtonChanged._1
                        |47.67 tzaiMiddleAndButtonChanged._2
                        |""".stripMargin.replaceAll("\\s", "")
  }

  it should "check the number of shifts for single characters" in {
    val jundaUnchanged: Double = countShifts(singleOut, Map(), Junda) //  7840
    val jundaTopChanged: Double = countShifts(singleOut, changeTopRow(), Junda)
    val jundaMiddleChanged: Double = countShifts(singleOut, changeMiddleRow(), Junda)
    val jundaButtomChanged: Double = countShifts(singleOut, changeButtomRow(), Junda)
    val jundaMiddleAndButtonChanged: Double = countShifts(singleOut, changeMiddleAndButtomRow(), Junda) //  7342

    val tzaiUnchanged: Double = countShifts(singleOutTzai, Map(), Tzai) //  7840
    val tzaiTopChanged: Double = countShifts(singleOutTzai, changeTopRow(), Tzai)
    val tzaiMiddleChanged: Double = countShifts(singleOutTzai, changeMiddleRow(), Tzai)
    val tzaiButtomChanged: Double = countShifts(singleOutTzai, changeButtomRow(), Tzai)
    val tzaiMiddleAndButtonChanged: Double = countShifts(singleOutTzai, changeMiddleAndButtomRow(), Tzai) //  7342

    val strRes = " " 
      + approximatelyEqual("jundaUnchanged", jundaUnchanged)
      + approximatelyEqual("jundaTopChanged", jundaTopChanged)
      + approximatelyEqual("jundaMiddleChanged", jundaMiddleChanged)
      + approximatelyEqual("jundaButtomChanged", jundaButtomChanged)
      + approximatelyEqual("jundaMiddleAndButtonChanged",jundaMiddleAndButtonChanged)
      + approximatelyEqual("tzaiUnchanged", tzaiUnchanged)
      + approximatelyEqual("tzaiTopChanged", tzaiTopChanged)
      + approximatelyEqual("tzaiMiddleChanged", tzaiMiddleChanged)
      + approximatelyEqual("tzaiButtomChanged", tzaiButtomChanged)
      + approximatelyEqual("tzaiMiddleAndButtonChanged", tzaiMiddleAndButtonChanged)
    
    val finalStr = strRes.replaceAll("\\s", "")
    finalStr shouldBe """  
                        |3.365 jundaUnchanged
                        |3.544 jundaTopChanged
                        |3.222 jundaMiddleChanged
                        |3.288 jundaButtomChanged
                        |3.544 jundaMiddleAndButtonChanged
                        |3.392 tzaiUnchanged
                        |3.531 tzaiTopChanged
                        |3.253 tzaiMiddleChanged
                        |3.133 tzaiButtomChanged
                        |3.563 tzaiMiddleAndButtonChanged
                        |""".stripMargin.replaceAll("\\s", "")
  }


  it should "check the number of shifts for multi character words" in {
    val jundaUnchanged: Double = countShifts(multiOut, Map(), Junda) //  7840
    val jundaMiddleChanged: Double = countShifts(multiOut, changeMiddleRow(), Junda)
    val jundaButtomChanged: Double = countShifts(multiOut, changeButtomRow(), Junda)
    val jundaMiddleAndButtonChanged: Double = countShifts(multiOut, changeMiddleAndButtomRow(), Junda) //  7342

    val tzaiUnchanged: Double = countShifts(multiOutTzai, Map(), Tzai) //  7840
    val tzaiMiddleChanged: Double = countShifts(multiOutTzai, changeMiddleRow(), Tzai)
    val tzaiButtomChanged: Double = countShifts(multiOutTzai, changeButtomRow(), Tzai)
    val tzaiMiddleAndButtonChanged: Double = countShifts(multiOutTzai, changeMiddleAndButtomRow(), Tzai) //  7342

    val strRes = ""
      + approximatelyEqual("jundaUnchanged", jundaUnchanged)
      + approximatelyEqual("jundaMiddleChanged", jundaMiddleChanged)
      + approximatelyEqual("jundaButtomChanged", jundaButtomChanged)
      + approximatelyEqual("jundaMiddleAndButtonChanged", jundaMiddleAndButtonChanged)
      + approximatelyEqual("tzaiUnchanged", tzaiUnchanged)
      + approximatelyEqual("tzaiMiddleChanged", tzaiMiddleChanged)
      + approximatelyEqual("tzaiButtomChanged", tzaiButtomChanged)
      + approximatelyEqual("tzaiMiddleAndButtonChanged", tzaiMiddleAndButtonChanged)
    
    val finalStr = strRes.replaceAll("\\s", "")
    finalStr shouldBe """43.94 jundaUnchanged
                        |43.96 jundaMiddleChanged
                        |43.72 jundaButtomChanged
                        |43.60 jundaMiddleAndButtonChanged
                        |39.17 tzaiUnchanged
                        |41.11 tzaiMiddleChanged
                        |39.19 tzaiButtomChanged
                        |40.24 tzaiMiddleAndButtonChanged
                        |""".stripMargin.replaceAll("\\s", "")
  }

  def addTuples(tuple1: (Double, Double), tuple2: (Double, Double)): (Double, Double) = {
    (tuple1._1 + tuple2._1, tuple1._2 + tuple2._2)
  }

  def approximatelyEqual(name: String, a: Double): String = {
    return getFirstFiveDigits(a) + " " + name + "\n"
  }

  def getFirstFiveDigits(number: Double): String = {
    // Convert the double to a String
    val numberAsString = number.toString
    // Ensure the number has at least four characters
    if (numberAsString.length < 5) return numberAsString
    // Return the first four characters
    numberAsString.substring(0, 5)
  }

  def countLeftRight(entries: Set[OutputEntry],
                     rowsChanged: Map[String, String],
                     charSystem: CharSystem): (Double, Double) = {
    val codes = convertCodes(entries, rowsChanged, charSystem)
    val calcShifts = calculateLeftRightDistribution(codes)
    calcShifts
  }
  
  def countLeftRightWithSelection(entries: Set[OutputEntry],
                                  rowsChanged: Map[String, String],
                                  charSystem: CharSystem): (Double, Double) = {
    val codes: List[(String, Double)] = convertCodes(entries, rowsChanged, charSystem)
    val calcShifts: (Double, Double) = calculateLeftRightDistributionWithLeftHandSelection(codes)
    calcShifts
  } 

  def countShifts(entries: Set[OutputEntry],
                  rowsChanged: Map[String, String],
                  charSystem: CharSystem): Double = {
    val codes: List[(String, Double)] = convertCodes(entries, rowsChanged, charSystem)
    val calcShifts: Double = calculateHandShifts(codes)
    calcShifts
  }

  def calculateLeftRightDistributionWithLeftHandSelection(codes: List[(String, Double)]): (Double, Double) = {
    var res: (Double, Double) = (0, 0)
    var previous: Char = 'z'
    for (eachcode <- codes) {
      val output: String = eachcode._1.zipWithIndex.map { case (char, index) =>
        if (OutputFrequencyTesting.leftHand.contains(char)) {
          res = addTuples(res, (eachcode._2, 0))
        } else if (OutputFrequencyTesting.rightHand.contains(char)) {
          res = addTuples(res, (0, eachcode._2))
        } else {
          if (char != 'z') {
            throw new Exception("unknown character")  
          }
        }
        if (index == eachcode._1.size-1) {
          res = addTuples(res, (eachcode._2, 0))
        }
      }.mkString
    }
    res
  }
  
  def calculateLeftRightDistribution(codes: List[(String, Double)]): (Double, Double) = {
    var res: (Double, Double) = (0, 0)
    var previous: Char = 'z'
    for (eachcode <- codes) {
      if (eachcode._1.size > 4) {
        val test = ""
      }
      val output: String = eachcode._1.zipWithIndex.map { case (char, index) =>
        if (OutputFrequencyTesting.leftHand.contains(char)) {
          res = addTuples(res, (eachcode._2, 0))
        } else if (OutputFrequencyTesting.rightHand.contains(char)) {
          res = addTuples(res, (0, eachcode._2))
        } else {
          if (char != 'z') {
            throw new Exception("unknown character")
          }
        }
      }.mkString
    }
    res
  }

  def calculateHandShifts(codes: List[(String, Double)]): Double = {
    var res: Double = 0
    var previous: Char = 'z'
    for (eachcode <- codes) {
      val output: String = eachcode._1.zipWithIndex.map { case (char, index) =>
        if (index > 0) {
          if (isShift(previous, char)) {
            res += eachcode._2
          }
        }
        previous = char
      }.mkString
    }
    res
  }

  def convertCodes(outputCodes: Set[OutputEntry],
                   changingCodeMap: Map[String, String],
                   charSystem: CharSystem): List[(String, Double)] = {
    val res: ListBuffer[(String, Double)] = ListBuffer()

    for (entry <- outputCodes) {
      for (eachcode <- entry.codes) {
        val output: String = eachcode.map { char =>
          changingCodeMap.getOrElse(char.toString,
            char.toString
          )
        }.mkString
        var dub: Double = 0
        if (charSystem.equals(CharSystem.Junda)) {
          val jundaHead = entry.jundaReverseOrderG.head
          dub = jundaHead.junda.get.frequency / entry.jundaReverseOrderG.size
        } else if (charSystem.equals(CharSystem.Tzai)) {
          val tzaiHead = entry.tzaiReverseOrderG.head
          dub = tzaiHead.tzai.get.frequency / entry.tzaiReverseOrderG.size
        } else {
          throw new Exception("unknown CharSystem")
        }
        if (dub <= 0) {
          throw new Exception("no double frequency detected")
        }
        res += ((output, dub))
      }
    }
    res.toList
  }

  def defaultMap(): Map[String, String] = {
    return Map[String, String]()
  }

  def isShift(previousChar: Char, currentChar: Char): Boolean = {
    if (OutputFrequencyTesting.rightHand(previousChar)) {
      if (OutputFrequencyTesting.leftHand(currentChar)) {
        return true
      } else {
        return false
      }
    } else {
      if (OutputFrequencyTesting.leftHand(currentChar)) {
        return false
      } else {
        return true
      }
    }
  }

  def calculateHandShifts(codes: List[String]): Long = {
    var res: Long = 0
    var previous: Char = 'z'
    for (eachcode <- codes) {
      val output: String = eachcode.zipWithIndex.map { case (char, index) =>
        if (index > 0) {
          if (isShift(previous, char)) {
            res += 1
          }
        }
        previous = char
      }.mkString
    }
    res
  }

  def changeTopRow():  Map[String, String]  = {
    val changingCodeMap: Map[String, String] = Map(
      "q" -> "p",
      "w" -> "o",
      "e" -> "i",
      "r" -> "u",
      "t" -> "y",
      "y" -> "t",
      "u" -> "r",
      "i" -> "e",
      "o" -> "w",
      "p" -> "q"
    )
    changingCodeMap
  }

  def changeMiddleRow():  Map[String, String]  = {
    val changingCodeMap: Map[String, String] = Map(
      "g" -> "h",
      "f" -> "j",
      "d" -> "k",
      "s" -> "l",
      "a" -> "m",
      "h" -> "g",
      "j" -> "f",
      "k" -> "d",
      "l" -> "s",
      "m" -> "a"
    )
    changingCodeMap
  }

  def changeButtomRow() = {
    val changingCodeMap: Map[String, String] = Map(
      "x" -> "n",
      "c" -> "b",
      "v" -> "v",
      "b" -> "c",
      "n" -> "x"
    )
    changingCodeMap
  }

  def changeMiddleAndButtomRow() = {
    val changingCodeMap: Map[String, String] = Map(
      "g" -> "h",
      "f" -> "j",
      "d" -> "k",
      "s" -> "l",
      "a" -> "m",
      "h" -> "g",
      "j" -> "f",
      "k" -> "d",
      "l" -> "s",
      "m" -> "a",

      "x" -> "n",
      "c" -> "b",
      "v" -> "v",
      "b" -> "c",
      "n" -> "x"
    )
    changingCodeMap
  }

}

object OutputFrequencyTesting {
  val strokeQWERTY: Set[Char] = Set(
    'q','w','e','r','t'
  )

  val strokeYUIOP: Set[Char] = Set(
    'y', 'u', 'i', 'o', 'p'
  )

  val strokeASDFG: Set[Char] = Set(
    'a', 's', 'd', 'f', 'g'
  )

  val strokeHJKLM: Set[Char] = Set(
    'h', 'j', 'k', 'l','m'
  )

  val strokeXCVBN: Set[Char] = Set(
    'x', 'c', 'v', 'b', 'n'
  )

  val leftHand: Set[Char] = Set(
    'q','w','e','r','t',
    'a','s','d','f','g',
    'x','c','v','b' //, 'z'
  )
  val rightHand: Set[Char] = Set(
    'y','u','i','o','p',
    'h','j','k','l','m',
    'n'
  )
}

