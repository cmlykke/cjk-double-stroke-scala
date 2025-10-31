package Atypes

import scala.jdk.StreamConverters.*
import scala.math.Ordering

class AsortingObject(val cedictPrimary: List[Boolean],
                     val cedictSecondary: List[Boolean],
                     val charsetPrimary: List[Int],
                     val charsetSecondary: List[Int],
                     val sortingCriteria: AsortingCriteria,
                     val hanchars: List[String],
                     val lettercode: String) extends Ordered[AsortingObject] {

  val cedictPrimaryComparison: Boolean = AsortingObject.cedictComparison(cedictPrimary)
  val cedictSecondaryComparison: Boolean = AsortingObject.cedictComparison(cedictSecondary)

  val charsetPrimaryComparison: List[Int] = AsortingObject.charsetComparison(charsetPrimary)
  val charsetSecondaryComparison: List[Int] = AsortingObject.charsetComparison(charsetSecondary)

  val sortingCriteriaComparison: Int = sortingCriteria.code
  val hancharComparison: List[Int] = AsortingObject.hancharComparison(hanchars)
  val lettercodeComparison: String = lettercode

  def compare(that: AsortingObject): Int = {
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