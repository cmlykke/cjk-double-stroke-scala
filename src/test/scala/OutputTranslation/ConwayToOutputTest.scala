package OutputTranslation

import UtilityClasses.Grapheme
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ConwayToOutputTest extends AnyFlatSpec with Matchers {


  it should "test single characters " in {

    val junda2tzai5 = ConwayToOutput.rawConwayToOutputCodes(Grapheme.splitIntoGraphemes("一").map(x => Grapheme(x)).toList)
    val junda3tzai2 = ConwayToOutput.rawConwayToOutputCodes(Grapheme.splitIntoGraphemes("是").map(x => Grapheme(x)).toList)
    val junda13 = ConwayToOutput.rawConwayToOutputCodes(Grapheme.splitIntoGraphemes("们").map(x => Grapheme(x)).toList)
    val junda4973tzai4927 = ConwayToOutput.rawConwayToOutputCodes(Grapheme.splitIntoGraphemes("仃").map(x => Grapheme(x)).toList)
    val junda5116tzai4419 = ConwayToOutput.rawConwayToOutputCodes(Grapheme.splitIntoGraphemes("芎").map(x => Grapheme(x)).toList)
    val junda5175tzai8365 = ConwayToOutput.rawConwayToOutputCodes(Grapheme.splitIntoGraphemes("囟").map(x => Grapheme(x)).toList)
    val junda6119tzai6974 = ConwayToOutput.rawConwayToOutputCodes(Grapheme.splitIntoGraphemes("跖").map(x => Grapheme(x)).toList)

    val tzai4976 = ConwayToOutput.rawConwayToOutputCodes(Grapheme.splitIntoGraphemes("卬").map(x => Grapheme(x)).toList)
    val tzai5153junda6852 = ConwayToOutput.rawConwayToOutputCodes(Grapheme.splitIntoGraphemes("汔").map(x => Grapheme(x)).toList)
    val tzai10493 = ConwayToOutput.rawConwayToOutputCodes(Grapheme.splitIntoGraphemes("繑").map(x => Grapheme(x)).toList)//繑

    junda2tzai5 shouldBe Set("h", "hzzz", "hzzzzz")
    junda3tzai2 shouldBe Set("xhjo", "xhjktz")
    junda13 shouldBe Set("ucg", "urg", "ucgz", "urgz", "ucgzzz", "urgzzz")
    junda4973tzai4927 shouldBe Set("uj", "ujzz", "ujzzzz")
    junda5116tzai4419 shouldBe Set("jxm", "jxmz", "njgg", "jjgg", "jxmzzz", "njggzz", "jjggzz")
    junda5175tzai8365 shouldBe Set("udtz", "udtzzz")
    junda6119tzai6974 shouldBe Set("jkxh", "xjjhug", "xjmkxh")

    tzai4976 shouldBe Set("pf", "pfzz", "pfzzzz")
    tzai5153junda6852 shouldBe Set("wkmz", "wkmzzz")
    tzai10493 shouldBe Set("uyog", "aroyog", "awwyog")

  }

  it should "test chars and words that cause errors" in {
    val testres1 = ConwayToOutput.rawConwayToOutputCodes(Grapheme.splitIntoGraphemes("譁").map(x => Grapheme(x)).toList)

    testres1 shouldBe Set("ijnj", "ijjj", "injj", "hhxhbj", "hhxhnj", "hhxjhj","thxhbj", "thxhnj", "thxjhj")
  }

  it should "test two character words" in {

    val testres1 = ConwayToOutput.rawConwayToOutputCodes(Grapheme.splitIntoGraphemes("你好").map(x => Grapheme(x)).toList)
    val testres2 = ConwayToOutput.rawConwayToOutputCodes(Grapheme.splitIntoGraphemes("泥孩").map(x => Grapheme(x)).toList)
    val testres3 = ConwayToOutput.rawConwayToOutputCodes(Grapheme.splitIntoGraphemes("問話").map(x => Grapheme(x)).toList)

    testres1 shouldBe Set("uodmn", "uod")
    testres2 shouldBe Set("wmflo", "wdflo", "wpflo", "wmf", "wpf", "wdf")
    testres3 shouldBe Set("pgiyg", "pgihg", "pgi")
  }

  it should "test three character words" in {

    val testres1 = ConwayToOutput.rawConwayToOutputCodes(Grapheme.splitIntoGraphemes("主人翁").map(x => Grapheme(x)).toList)
    val testres2 = ConwayToOutput.rawConwayToOutputCodes(Grapheme.splitIntoGraphemes("啞巴虧").map(x => Grapheme(x)).toList)
    val testres3 = ConwayToOutput.rawConwayToOutputCodes(Grapheme.splitIntoGraphemes("CP值").map(x => Grapheme(x)).toList) // this one is made up

    testres1 shouldBe Set("tooiz", "tootz")
    testres2 shouldBe Set("xfmnm")
    testres3 shouldBe Set("zzuhz")
  }

  it should "test four character words" in {

    val testres1 = ConwayToOutput.rawConwayToOutputCodes(Grapheme.splitIntoGraphemes("寶潔公司").map(x => Grapheme(x)).toList)
    val testres2 = ConwayToOutput.rawConwayToOutputCodes(Grapheme.splitIntoGraphemes("心砰砰跳").map(x => Grapheme(x)).toList)
    val testres3 = ConwayToOutput.rawConwayToOutputCodes(Grapheme.splitIntoGraphemes("502胶").map(x => Grapheme(x)).toList) // this one is made up

    testres1 shouldBe Set("wwogg")
    testres2 shouldBe Set("qkkjo")
    testres3 shouldBe Set("zzzpo")
  }


  it should "test five character words" in {

    val testres1 = ConwayToOutput.rawConwayToOutputCodes(Grapheme.splitIntoGraphemes("抗耐甲氧西林金葡菌").map(x => Grapheme(x)).toList)
    val testres2 = ConwayToOutput.rawConwayToOutputCodes(Grapheme.splitIntoGraphemes("攻击型核潜艇").map(x => Grapheme(x)).toList)
    val testres3 = ConwayToOutput.rawConwayToOutputCodes(Grapheme.splitIntoGraphemes("21三体综合症").map(x => Grapheme(x)).toList) // this one is made up

    testres1 shouldBe Set("lkxyj")
    testres2 shouldBe Set("jhhdw")
    testres3 shouldBe Set("zzhua")
  }

}
