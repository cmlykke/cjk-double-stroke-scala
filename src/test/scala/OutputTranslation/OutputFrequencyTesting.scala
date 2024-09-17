package OutputTranslation

import UtilityClasses.CharSystem.{Junda, Tzai}
import UtilityClasses.{CedictEntry, CharSystem, OutputEntry}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import staticFileGenerators.cedictMap.GenerateCedictMap

import scala.collection.mutable.ListBuffer

class OutputFrequencyTesting extends AnyFlatSpec with Matchers {

  val singleOut: Set[OutputEntry] = OutputTranslation.jundaSingelOut
  val multiOut: Set[OutputEntry] = OutputTranslation.jundaMultiOut
  val singleOutTzai: Set[OutputEntry] = OutputTranslation.tzaiSingelOut
  val multiOutTzai: Set[OutputEntry] = OutputTranslation.tzaiMultiOut

  it should "check the number of uses of left vs right hand - single characters" in {

    val jundaUnchanged: (Double, Double) = countLeftRight(singleOut, Map(), Junda)
    val jundaTopChanged: (Double, Double) = countLeftRight(singleOut, changeTopRow(), Junda)
    val jundaMiddleChanged: (Double, Double) = countLeftRight(singleOut, changeMiddleRow(), Junda)
    val jundaButtomChanged: (Double, Double) = countLeftRight(singleOut, changeButtomRow(), Junda)
    val jundaMiddleAndButtonChanged: (Double, Double) = countLeftRight(singleOut, changeMiddleAndButtomRow(), Junda)

    val tzaiUnchanged: (Double, Double) = countLeftRight(singleOutTzai, Map(), Tzai)
    val tzaiTopChanged: (Double, Double) = countLeftRight(singleOutTzai, changeTopRow(), Tzai)
    val tzaiMiddleChanged: (Double, Double) = countLeftRight(singleOutTzai, changeMiddleRow(), Tzai)
    val tzaiButtomChanged: (Double, Double) = countLeftRight(singleOutTzai, changeButtomRow(), Tzai)
    val tzaiMiddleAndButtonChanged: (Double, Double) = countLeftRight(singleOutTzai, changeMiddleAndButtomRow(), Tzai)

    //junda
    approximatelyEqual(jundaUnchanged._1, 3.174) == true
    approximatelyEqual(jundaUnchanged._2, 2.535) == true

    approximatelyEqual(jundaTopChanged._1, 3.439) == true
    approximatelyEqual(jundaTopChanged._2, 2.270) == true

    approximatelyEqual(jundaMiddleChanged._1, 2.569) == true
    approximatelyEqual(jundaMiddleChanged._2, 3.140) == true

    approximatelyEqual(jundaButtomChanged._1, 3.121) == true
    approximatelyEqual(jundaButtomChanged._2, 2.588) == true

    approximatelyEqual(jundaMiddleAndButtonChanged._1, 2.516) == true
    approximatelyEqual(jundaMiddleAndButtonChanged._2, 3.193) == true

    //tzai
    approximatelyEqual(tzaiUnchanged._1, 3.407) == true
    approximatelyEqual(tzaiUnchanged._2, 2.933) == true

    approximatelyEqual(tzaiTopChanged._1, 3.948) == true
    approximatelyEqual(tzaiTopChanged._2, 2.392) == true

    approximatelyEqual(tzaiMiddleChanged._1, 2.623) == true
    approximatelyEqual(tzaiMiddleChanged._2, 3.717) == true

    approximatelyEqual(tzaiButtomChanged._1, 3.418) == true
    approximatelyEqual(tzaiButtomChanged._2, 2.922) == true

    approximatelyEqual(tzaiMiddleAndButtonChanged._1, 2.634) == true
    approximatelyEqual(tzaiMiddleAndButtonChanged._2, 3.706) == true
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
    approximatelyEqual(jundaUnchanged._1, 299.574) == true
    approximatelyEqual(jundaUnchanged._2, 227.931) == true

    approximatelyEqual(jundaTopChanged._1, 319.991) == true
    approximatelyEqual(jundaTopChanged._2, 207.514) == true

    approximatelyEqual(jundaMiddleChanged._1, 226.641) == true
    approximatelyEqual(jundaMiddleChanged._2, 300.863) == true

    approximatelyEqual(jundaButtomChanged._1, 299.663) == true
    approximatelyEqual(jundaButtomChanged._2, 227.842) == true

    approximatelyEqual(jundaMiddleAndButtonChanged._1, 226.730) == true
    approximatelyEqual(jundaMiddleAndButtonChanged._2, 300.774) == true

    //tzai
    approximatelyEqual(tzaiUnchanged._1, 326.095) == true
    approximatelyEqual(tzaiUnchanged._2, 261.326) == true

    approximatelyEqual(tzaiTopChanged._1, 379.638) == true
    approximatelyEqual(tzaiTopChanged._2, 207.783) == true

    approximatelyEqual(tzaiMiddleChanged._1, 230.378) == true
    approximatelyEqual(tzaiMiddleChanged._2, 357.042) == true

    approximatelyEqual(tzaiButtomChanged._1, 324.173) == true
    approximatelyEqual(tzaiButtomChanged._2, 263.248) == true

    approximatelyEqual(tzaiMiddleAndButtonChanged._1, 228.457) == true
    approximatelyEqual(tzaiMiddleAndButtonChanged._2, 358.964) == true
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

    approximatelyEqual(jundaUnchanged, 2.226) == true
    approximatelyEqual(jundaTopChanged, 2.197) == true
    approximatelyEqual(jundaMiddleChanged, 2.055) == true
    approximatelyEqual(jundaButtomChanged, 2.020) == true
    approximatelyEqual(jundaMiddleAndButtonChanged, 2.183) == true

    approximatelyEqual(tzaiUnchanged, 2.440) == true
    approximatelyEqual(tzaiTopChanged, 2.409) == true
    approximatelyEqual(tzaiMiddleChanged, 2.185) == true
    approximatelyEqual(tzaiButtomChanged, 2.232) == true
    approximatelyEqual(tzaiMiddleAndButtonChanged, 2.431) == true
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

    approximatelyEqual(jundaUnchanged, 202.358) == true
    approximatelyEqual(jundaMiddleChanged, 206.332) == true
    approximatelyEqual(jundaButtomChanged, 201.408) == true
    approximatelyEqual(jundaMiddleAndButtonChanged, 211.556) == true

    approximatelyEqual(tzaiUnchanged, 222.669) == true
    approximatelyEqual(tzaiMiddleChanged, 202.353) == true
    approximatelyEqual(tzaiButtomChanged, 224.580) == true
    approximatelyEqual(tzaiMiddleAndButtonChanged, 226.667) == true
  }

  def addTuples(tuple1: (Double, Double), tuple2: (Double, Double)): (Double, Double) = {
    (tuple1._1 + tuple2._1, tuple1._2 + tuple2._2)
  }

  def approximatelyEqual(a: Double, b: Double, tolerance: Double = 1e-6): Boolean = {
    math.abs(a - b) <= tolerance
  }

  def countLeftRight(entries: Set[OutputEntry],
                     rowsChanged: Map[String, String],
                     charSystem: CharSystem): (Double, Double) = {
    val codes: List[(String, Double)] = convertCodes(entries, rowsChanged, charSystem)
    val calcShifts: (Double, Double) = calculateLeftRightDistribution(codes)
    calcShifts
  }

  def countShifts(entries: Set[OutputEntry],
                  rowsChanged: Map[String, String],
                  charSystem: CharSystem): Double = {
    val codes: List[(String, Double)] = convertCodes(entries, rowsChanged, charSystem)
    val calcShifts: Double = calculateHandShifts(codes)
    calcShifts
  }

  def calculateLeftRightDistribution(codes: List[(String, Double)]): (Double, Double) = {
    var res: (Double, Double) = (0, 0)
    var previous: Char = 'z'
    for (eachcode <- codes) {
      val output: String = eachcode._1.zipWithIndex.map { case (char, index) =>
        if (OutputFrequencyTesting.leftHand.contains(char)) {
          res = addTuples(res, (eachcode._2, 0))
        } else if (OutputFrequencyTesting.rightHand.contains(char)) {
          res = addTuples(res, (0, eachcode._2))
        } else {
          throw new Exception("unknown character")
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
          val jundaHead = entry.jundaReverseOrder.head
          dub = jundaHead.junda.get.frequency / entry.jundaReverseOrder.size
        } else if (charSystem.equals(CharSystem.Tzai)) {
          val tzaiHead = entry.tzaiReverseOrder.head
          dub = tzaiHead.tzai.get.frequency / entry.tzaiReverseOrder.size
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
    'x','c','v','b',
    'z'
  )
  val rightHand: Set[Char] = Set(
    'y','u','i','o','p',
    'h','j','k','l','m',
    'n'
  )
}

