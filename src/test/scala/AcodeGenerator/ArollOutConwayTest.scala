package AcodeGenerator

import AcodeGenerators.ArollOutConway
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ArollOutConwayTest extends AnyFlatSpec with Matchers {


  it should "test that correct conway can be found and split" in {

    // 述
    //(1234|1235)4(454|4454|4554)
    //1234
    val test1: Set[String] = ArollOutConway.getRemainderFromMainAndElem(
      List("(1234|1235)4(454|4454|4554)"), List("1234"))
    test1 shouldBe Set("4(454|4454|4554)")

    //飲
    //34(1|4)511543534
    //34(1|4)(51154|511211)
    val test2: Set[String] = ArollOutConway.getRemainderFromMainAndElem(
      List("34(1|4)511543534"), List("34(1|4)(51154|511211)"))
    test2 shouldBe Set("3534")

    //竹
    //312312
    //
    val test3: Set[String] = ArollOutConway.getRemainderFromMainAndElem(
      List("312312"), List("312312"))
    test3 shouldBe Set("")

    //煛
    //43342511125111
    //25111251114334
    //25111
    val test4: Set[String] = ArollOutConway.getRemainderFromMainAndElem(
      List("43342511125111", "25111251114334"), List("25111"))
    test4 shouldBe Set("251114334")


    //𧾷
    //251(215|2121)
    //2512121
    val test5: Set[String] = ArollOutConway.getRemainderFromMainAndElem(
      List("251(215|2121)"), List("2512121"))
    test5 shouldBe Set("")

  }

  it should "test rollout codes" in {

    val test1 = ArollOutConway.roolOutConway("123(34|45)111")
    test1 shouldBe Set("12345111","12334111")

    val test2 = ArollOutConway.roolOutConway("123(|45)111")
    test2 shouldBe Set("12345111", "123111")
  }

  it should "test ability to get head of raw conway" in {

    val test1: Set[(String, String)] = ArollOutConway.getHeadCodeFromRawConway(
      "24(1|35)4(454|4454|4554)")
    test1 shouldBe Set(("24","(1|35)4(454|4454|4554)"))

    val test2: Set[(String, String)] = ArollOutConway.getHeadCodeFromRawConway(
      "(1|35)4(454|4454|4554)")
    test2 shouldBe Set(("14", "(454|4454|4554)"),("35", "4(454|4454|4554)"))

    val test3: Set[(String, String)] = ArollOutConway.getHeadCodeFromRawConway(
      "4(1|35)4(454|4454|4554)")
    test3 shouldBe Set(("41", "4(454|4454|4554)"),("43", "54(454|4454|4554)"))

    val test4: Set[(String, String)] = ArollOutConway.getHeadCodeFromRawConway(
      "(1|)4(53|4454|4554)")
    test4 shouldBe Set(("14", "(53|4454|4554)"),("45", "3"),("44", "454"),("44", "554"))

  }

  it should "test ability to get tail of raw conway" in {

    val test1: Set[(String, String)] = ArollOutConway.getTailCodeFromRawConway(
      "(4554|4544|454)4(53|1)42")
    test1 shouldBe Set(("42","(4554|4544|454)4(53|1)"))

    val test2: Set[(String, String)] = ArollOutConway.getTailCodeFromRawConway(
      "(4554|4544|454)4(53|1)")
    test2 shouldBe Set(("41","4554"),("41","4544"),("41","454"),("53","45544"), ("53","45444"), ("53","4544"))

    val test3: Set[(String, String)] = ArollOutConway.getTailCodeFromRawConway(
      "(4554|4544|454)4(53|1)4"
    )
    test3 shouldBe Set(("14","45544"),("14","45444"),("14","4544"), ("34","455445"),("34","454445"),("34","45445"))

    val test4: Set[(String, String)] = ArollOutConway.getTailCodeFromRawConway(
      "(4554|4544|35)4(1|)"
    )
    test4 shouldBe Set(("41","35"),("41","4544"),("41","4554"), ("54","3"),("44","455"),("44","454"))

  }


}
