package UtilityClasses

import UtilityClasses.InputSizes.Three_oneAndFive_one
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import staticFileGenerators.Conway.GenerateConwayCodes

class ConwayTest  extends AnyFlatSpec with Matchers {

  "test the split conway into pairs funciton" should
    "a list of conway string should be split into paris" in {
    val raw1 = "3511(252|522)52"
    val raw2 = "34(152|154|454)"
    val raw3 = "(12|34)(56|78)9"
    val raw4 = "12(3|)45"
    val raw5 = "12(|3)45"

    val test1 = Conway(raw1)
    val test2 = Conway(raw2)
    val test3 = Conway(raw3)
    val test4 = Conway(raw4)
    val test5 = Conway(raw5)

    val test1ListsData = test1Lists()

    val res1: Set[ConwayUnambigous] = test1.getSplitConwayList(Three_oneAndFive_one)
    res1.size shouldBe 4
    res1 shouldEqual test1ListsData
    /*
    res1 shouldBe
      Set(
        List("35", "11", "25", "25", "2"),
        List("35", "11", "52", "25", "2"))*/
  }

  private def test1Lists(): Set[ConwayUnambigous] = {
    // Create four lists of strings
    val list1: List[String] = List("35", "11", "25", "25", "2")
    val list2: List[String] = List("35", "11", "25", "52")
    val list3: List[String] = List("35", "11", "52", "25", "2")
    val list4: List[String] = List("35", "11", "52", "52")

    // Create a Set that contains these lists
    val setOfLists: Set[ConwayUnambigous] =
      Set(ConwayUnambigous(conwayPairs = list1, is4Code = false),
        ConwayUnambigous(conwayPairs = list2, is4Code = true),
        ConwayUnambigous(conwayPairs = list3, is4Code = false),
        ConwayUnambigous(conwayPairs = list4, is4Code = true))



    setOfLists
  }

}
