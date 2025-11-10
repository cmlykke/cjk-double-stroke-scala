package Atypes

import scala.collection.immutable
import scala.jdk.StreamConverters.*
import scala.math.Ordering

class AsortingObject(val cedictPrimary: List[Boolean],
                     val cedictSecondary: List[Boolean],
                     val charsetPrimary: List[Int],
                     val charsetSecondary: List[Int],
                     val bcluData: immutable.HashMap[String, Int],
                     val sinicaData: immutable.HashMap[String, Int],
                     val sortingCriteria: AsortingCriteria,
                     val hanchars: List[String],
                     val lettercode: String) extends Ordered[AsortingObject] {


  val cedictPrimaryComparison: Boolean = AsortingObject.cedictComparison(cedictPrimary)
  val cedictSecondaryComparison: Boolean = AsortingObject.cedictComparison(cedictSecondary)

  val charsetPrimaryComparison: List[Int] = AsortingObject.charsetComparison(charsetPrimary)
  val charsetSecondaryComparison: List[Int] = AsortingObject.charsetComparison(charsetSecondary)

  val bcluAndSinicaPrimary: Int = AsortingObject.wordFreq(hanchars, bcluData)
  val bcluAndSinicaSecondary: Int = AsortingObject.wordFreq(hanchars, sinicaData)

  val sortingCriteriaComparison: Int = sortingCriteria.code
  val hancharComparison: List[Int] = AsortingObject.hancharComparison(hanchars)
  val lettercodeComparison: String = lettercode


  val sortingString: String = AsortingObject.generateSortingString(
    hanchars,lettercode,
    cedictPrimaryComparison,cedictSecondaryComparison,charsetPrimaryComparison,charsetSecondaryComparison,
    bcluAndSinicaPrimary, bcluAndSinicaSecondary,
    sortingCriteriaComparison,hancharComparison,lettercodeComparison)

  val test = ""

  def compare(that: AsortingObject): Int = {

    return this.sortingString.compare(that.sortingString)

    // Helper for boolean fields where true < false (true first in ascending sort)
    if (this.hanchars.length == 1 && this.hanchars.head == "木") {
      val test = ""
    }

    def boolCompare(b1: Boolean, b2: Boolean): Int = {
      if (b1 == b2) 0 else if (b1) -1 else 1
    }

    // Helper for List[Int] lexicographical comparison (smaller first, element-by-element)
    def listCompare(l1: List[Int], l2: List[Int]): Int = {
      Ordering.Iterable[Int].compare(l1, l2)
    }


    val c1 = AsortingObject.lettercodeComparison(this.lettercodeComparison, that.lettercodeComparison)
    if (c1 != 0) return math.signum(c1)

    if (this.hanchars.length == 1 && this.hanchars.head == "木") {
      val test = ""
    }

    // Chain comparisons in priority order (adjust priorities as needed based on your sorting logic)
    val c0 = Ordering.Int.compare(this.sortingCriteriaComparison, that.sortingCriteriaComparison)
    if (c0 != 0) return math.signum(c0)

    val c3 = boolCompare(this.cedictPrimaryComparison, that.cedictPrimaryComparison)
    if (c3 != 0) return math.signum(c3)

    val c5 = listCompare(this.charsetPrimaryComparison, that.charsetPrimaryComparison)
    if (c5 != 0) return math.signum(c5)

    val c6 = listCompare(this.charsetSecondaryComparison, that.charsetSecondaryComparison)
    if (c6 != 0) return math.signum(c6)

    val c4 = boolCompare(this.cedictSecondaryComparison, that.cedictSecondaryComparison)
    if (c4 != 0) return math.signum(c4)

    val c2 = listCompare(this.hancharComparison, that.hancharComparison)
    if (c2 != 0) return math.signum(c2)

    return 0
  }
}

object AsortingObject {

  def generateSortingString(
    hanchars: List[String],
    lettercode: String,
    cedictPrimaryComparison: Boolean,
    cedictSecondaryComparison: Boolean,
    charsetPrimaryComparison: List[Int],
    charsetSecondaryComparison: List[Int],
    bcluAndSinicaPrimary: Int,
    bcluAndSinicaSecondary: Int,
    sortingCriteriaComparison: Int,
    hancharComparison: List[Int],
    lettercodeComparison: String): String = {

    val characterList: String =  hanchars.mkString("")

    if (sortingCriteriaComparison > 9) {
      throw new RuntimeException("Sorting criteria is greater than 9")
    }
    var criteriaStr: String = "Cri:" + sortingCriteriaComparison.toString

    var cedictPrim: String = "CedictPrim:2"
    if (cedictPrimaryComparison == true) {
      cedictPrim = "CedictPrim:1"
    } else {
      cedictPrim = "CedictPrim:2"
    }

    var cedictSec: String = "CedictSec:2"
    if (cedictSecondaryComparison == true) {
      cedictSec = "CedictSec:1"
    } else {
      cedictSec = "CedictSec:2"
    }

    var mergedCharsetPrim: String = ""
    for (eachInt <- charsetPrimaryComparison) {
      if (eachInt == Int.MaxValue) {
        mergedCharsetPrim = mergedCharsetPrim + "99999" + ","
      } else {
        mergedCharsetPrim = mergedCharsetPrim + f"$eachInt%05d" + ","
      }
    }

    var mergedCharsetSec: String = ""
    for (eachInt <- charsetSecondaryComparison) {
      if (eachInt == Int.MaxValue) {
        mergedCharsetSec = mergedCharsetSec + "99999" + ","
      } else {
        mergedCharsetSec = mergedCharsetSec + f"$eachInt%05d" + ","
      }
    }

    var mergedbcluAndSinicaPrim: String = ""
    
      if (bcluAndSinicaPrimary == Int.MaxValue) {
        mergedbcluAndSinicaPrim = mergedbcluAndSinicaPrim + "Word:" + "9999999" + ","
      } else {
        mergedbcluAndSinicaPrim = mergedbcluAndSinicaPrim + "Word:" + f"$bcluAndSinicaPrimary%07d" + ","
      }
    

    var mergedbcluAndSinicaSec: String = ""
    
      if (bcluAndSinicaSecondary == Int.MaxValue) {
        mergedbcluAndSinicaSec = mergedbcluAndSinicaSec + "Word:" + "9999999" + ","
      } else {
        mergedbcluAndSinicaSec = mergedbcluAndSinicaSec + "Word:" + f"$bcluAndSinicaSecondary%07d" + ","
      }
    

    val outout = criteriaStr + "." +
      cedictPrim + "." + mergedCharsetPrim + "." + mergedbcluAndSinicaPrim + "."
      + cedictSec + "." + mergedCharsetSec + "." + mergedbcluAndSinicaSec + "." +  characterList

    return outout
  }
//Cri:6.CedictPrim:1.00004,00088,00294,.Word:0000004,Word:0000088,Word:0000294,.CedictSec:2.00003,00148,99999,.Word:0000003,Word:0000148,Word:9999999,.不像样
  def lettercodeComparison(lettercodeThis: String, lettercodeThat: String): Int = {
    val lenThis = lettercodeThis.length
    val lenThat = lettercodeThat.length
    if (lenThis < lenThat) {
      return -1
    }
    else if (lenThis > lenThat) {
      return 1
    }
    else {
      val stringcomparison = lettercodeThis.compareTo(lettercodeThat)
      return stringcomparison
    }
  }

  def wordFreq(inputWord: List[String], freq: immutable.HashMap[String, Int]): Int = {
    val getVal = freq.get(inputWord.mkString(""))
    if (getVal.isEmpty) {
      return Int.MaxValue
    } else {
      return getVal.get
    }
  } 

  def charsetComparison(charset: List[Int]): List[Int] = {
    charset.sorted
  }

  def cedictComparison(cedict: List[Boolean]): Boolean = {
    !cedict.contains(false)
  }

  def hancharComparison(hanchars: List[String]): List[Int] = {
    hanchars.flatMap(_.codePoints().toArray).sorted.toList
  }
}