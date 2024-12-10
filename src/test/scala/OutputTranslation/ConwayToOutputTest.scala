package OutputTranslation

import ElementGenerator.ElementList
import UtilityClasses.{Grapheme, OutputEntry}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ConwayToOutputTest extends AnyFlatSpec with Matchers {


  it should "test all elements" in {
    val conwaySet: Set[OutputEntry] = OutputSorting.conFull
    val elemSet: Set[String] = ElementList.elementSet
    val elementSet: Set[OutputEntry] = conwaySet.filter(x => elemSet.contains(x.chineseStr))
    var testres: String = ""

    testres += oToT(elementSet.filter(x => x.chineseStr == "木").head)
    testres += oToT(elementSet.filter(x => x.chineseStr == "⽊").head)
    testres += oToT(elementSet.filter(x => x.chineseStr == "足").head)
    testres += oToT(elementSet.filter(x => x.chineseStr == "⻊").head)
    testres += oToT(elementSet.filter(x => x.chineseStr == "⾜").head)

    testres += oToT(elementSet.filter(x => x.chineseStr == "竹").head)
    testres += oToT(elementSet.filter(x => x.chineseStr == "⺮").head)
    testres += oToT(elementSet.filter(x => x.chineseStr == "虫").head)
    testres += oToT(elementSet.filter(x => x.chineseStr == "⾍").head)
    testres += oToT(elementSet.filter(x => x.chineseStr == "手").head)

    testres += oToT(elementSet.filter(x => x.chineseStr == "⼿").head)
    testres += oToT(elementSet.filter(x => x.chineseStr == "扌").head)
    testres += oToT(elementSet.filter(x => x.chineseStr == "⺘").head)
    testres += oToT(elementSet.filter(x => x.chineseStr == "目").head)
    testres += oToT(elementSet.filter(x => x.chineseStr == "言").head)
    
    testres += oToT(elementSet.filter(x => x.chineseStr == "訁").head)
    testres += oToT(elementSet.filter(x => x.chineseStr == "⾔").head)
    testres +=  oToT(elementSet.filter(x => x.chineseStr == "糸").head)
    testres += oToT(elementSet.filter(x => x.chineseStr == "糹").head)
    testres += oToT(elementSet.filter(x => x.chineseStr == "⺯").head)
    
    testres += oToT(elementSet.filter(x => x.chineseStr == "⽷").head)
    testres += oToT(elementSet.filter(x => x.chineseStr == "金").head)
    testres += oToT(elementSet.filter(x => x.chineseStr == "⾦").head)
    testres += oToT(elementSet.filter(x => x.chineseStr == "門").head)
    testres += oToT(elementSet.filter(x => x.chineseStr == "⾨").head)
    
    testres += oToT(elementSet.filter(x => x.chineseStr == "馬").head)
    testres += oToT(elementSet.filter(x => x.chineseStr == "⾺").head)
    testres += oToT(elementSet.filter(x => x.chineseStr == "食").head)
    testres += oToT(elementSet.filter(x => x.chineseStr == "飠").head)
    testres += oToT(elementSet.filter(x => x.chineseStr == "⾷").head)
    
    testres += oToT(elementSet.filter(x => x.chineseStr == "⻝").head)
    testres += oToT(elementSet.filter(x => x.chineseStr == "⻞").head)
    testres += oToT(elementSet.filter(x => x.chineseStr == "⻟").head)
    testres += oToT(elementSet.filter(x => x.chineseStr == "車").head)
    testres += oToT(elementSet.filter(x => x.chineseStr == "⾞").head)

    allStringsOccur(elemSet, testres) shouldBe true
    
    testres.replaceAll("\\s", "") shouldBe 
    """(木,List(26408),Set(jozz, dzzz, jozzzz, d))
      |(⽊,List(12106),Set(jozz, dzzz, jozzzz, d))
      |(足,List(36275),HashSet(j, jzzz, xjjhzz, xjjh, xjmz, xjmzzz))
      |(⻊,List(11978),Set(xjjh, jzzz, xjjhzz, j))
      |(⾜,List(12188),HashSet(j, jzzz, xjjhzz, xjjh, xjmz, xjmzzz))
      |(竹,List(31481),Set(yelz, fzzz, yelzzz, f))
      |(⺮,List(11950),Set(yelz, fzzz, yelzzz, f))
      |(虫,List(34411),Set(xjlz, szzz, xjlzzz, s))
      |(⾍,List(12173),Set(xjlz, szzz, xjlzzz, s))
      |(手,List(25163),Set(yjzz, lzzz, yjzzzz, l))
      |(⼿,List(12095),Set(yjzz, lzzz, yjzzzz, l))
      |(扌,List(25164),Set(jhzz, lzzz, jhzzzz, l))
      |(⺘,List(11928),Set(jhzz, lzzz, jhzzzz, l))
      |(目,List(30446),Set(xhhz, kzzz, xhhzzz, k))
      |(言,List(35328),HashSet(hhxh, thxhzz, i, thxh, hhxhzz, izzz))
      |(訁,List(35329),HashSet(hhxh, thxhzz, i, thxh, hhxhzz, izzz))
      |(⾔,List(12180),HashSet(hhxh, thxhzz, i, thxh, hhxhzz, izzz))
      |(糸,List(31992),HashSet(awwz, uzzz, u, aroz, awwzzz, arozzz))
      |(糹,List(31993),HashSet(awwz, uzzz, u, aroz, awwzzz, arozzz))
      |(⺯,List(11951),HashSet(awwz, uzzz, u, aroz, awwzzz, arozzz))
      |(⽷,List(12151),HashSet(awwz, uzzz, u, aroz, awwzzz, arozzz))
      |(金,List(37329),Set(ohcy, rzzz, ohcyzz, r))
      |(⾦,List(12198),Set(ohcy, rzzz, ohcyzz, r))
      |(門,List(38272),Set(xhxh, pzzz, xhxhzz, p))
      |(⾨,List(12200),Set(xhxh, pzzz, xhxhzz, p))
      |(馬,List(39340),HashSet(jhxw, nhxwwz, nhxw, w, wzzz, jhxwwz))
      |(⾺,List(12218),HashSet(jhxw, nhxwwz, nhxw, w, wzzz, jhxwwz))
      |(食,List(39135),HashSet(oqhs, ozzz, oqhnhz, omhh, omhs, oqhh, oqhszz, omhszz, omhnhz, o))
      |(飠,List(39136),HashSet(oqhs, ozzz, oqhnhz, omhh, omhs, oqhh, oqhszz, omhszz, omhnhz, o))
      |(⾷,List(12215),HashSet(oqhs, ozzz, oqhnhz, omhh, omhs, oqhh, oqhszz, omhszz, omhnhz, o))
      |(⻝,List(11997),HashSet(oqhs, ozzz, oqhnhz, omhh, omhs, oqhh, oqhszz, omhszz, omhnhz, o))
      |(⻞,List(11998),HashSet(oqhs, ozzz, oqhnhz, omhh, omhs, oqhh, oqhszz, omhszz, omhnhz, o))
      |(⻟,List(11999),HashSet(oqhs, ozzz, oqhnhz, omhh, omhs, oqhh, oqhszz, omhszz, omhnhz, o))
      |(車,List(36554),Set(jghn, ezzz, jghnzz, e))
      |(⾞,List(12190),Set(jghn, ezzz, jghnzz, e))
      |""".stripMargin.replaceAll("\\s", "")
    
  }

  private def oToT(input: OutputEntry): String = {
    val res: (String, List[String], Set[String]) = (input.chineseStr, input.chineseStr.toList.map(_.toInt.toString), input.codes)
    return res.toString() + "\n"
  }

  private def allStringsOccur(strSet: Set[String], str: String): Boolean = {
    for (eachElem <- strSet) {
      val isGraph = Grapheme.isGrapheme(eachElem)
      val doesoccur = str.contains(eachElem)
      if (!doesoccur && isGraph) {
        return false;
      }
    }
    return true;
  }

  /*
  new ElementType("1234", new Cluster("木"), "a", "木"),
    new ElementType("1234", new Cluster("⽊"), "a", "⽊"),
    //overlap point: Junda 2919梗  Tzai
    new ElementType("251(215|2121)", new Cluster("⿱口止"), "b", "⿱口止"),
    new ElementType("251(215|2121)", new Cluster("足"), "b", "足"),// 足
    new ElementType("2512121", new Cluster("⻊"), "b", "⻊"),
    new ElementType("251(215|2121)", new Cluster("⾜"), "b", "⾜"),
    //overlap point: Junda 2977趴  Tzai
    new ElementType("314314", new Cluster("⿰⿱𠂊亅⿱𠂊亅"), "c", "⿰⿱𠂊亅⿱𠂊亅"),
    new ElementType("314314", new Cluster("竹"), "c", "竹"), //'竹'
    new ElementType("314314", new Cluster("⺮"), "c", "⺮"), //'⺮' 11950
    //overlap point: Junda 3785筏  Tzai
    new ElementType("251214", new Cluster("虫"), "d", "虫"),
    new ElementType("251214", new Cluster("⾍"), "d", "⾍"),
    //overlap point: Junda 3918蜈  Tzai
    new ElementType("3112", new Cluster("手"), "e", "手"),
    new ElementType("3112", new Cluster("⼿"), "e", "⼿"),
    new ElementType("121", new Cluster("扌"), "e", "扌"),
    new ElementType("121", new Cluster("⺘"), "e", "⺘"),
    //overlap point: Junda 4299摞  Tzai
    new ElementType("25111", new Cluster("目"), "f", "目"),
    //overlap point: Junda 4742嗪 -- 5846瞋  Tzai --目  25111(12251111|13251111|15251115|35251115|53251115)34

    //new ElementType("251", new Cluster("口"), "g"),
    //嗉 5351 -- 題 5105
    //new ElementType("122111", new Cluster("耳"), "h"),
    //聩 5209 -- 122111251212534  耳
    //next highest overlap: 糇 junda 5898

    new ElementType("(1|4)111251", new Cluster("言"), "k", "言"),
    new ElementType("(1|4)111251", new Cluster("訁"), "k", "訁"),
    new ElementType("(1|4)111251", new Cluster("⾔"), "k", "⾔"),
    //overlap point: Junda   tzai 誠  894
    new ElementType("(554234|554444)", new Cluster("⿱⿰②丶③"), "l", "⿱⿰②丶③"),
    new ElementType("(554234|554444)", new Cluster("糸"), "l", "糸"),
    new ElementType("(554234|554444)", new Cluster("糹"), "l", "糹"),
    new ElementType("(554234|554444)", new Cluster("⺯"), "l", "⺯"),
    new ElementType("(554234|554444)", new Cluster("⽷"), "l", "⽷"),
    //overlap point: Junda   tzai  縱 1448  (554234|554444)33234342134   ⿱⿰②丶③
    new ElementType("34112431", new Cluster("⿱人⿻⿱一⿱十一丷"), "m", "⿱人⿻⿱一⿱十一丷"),
    new ElementType("34112431", new Cluster("金"), "m", "金"),
    new ElementType("34112431", new Cluster("⾦"), "m", "⾦"),
    //overlap point: Junda   tzai 錶 2473  // ⿱人⿻⿱一⿱十一丷   3411243111213534
    new ElementType("25112511", new Cluster("⿰𠁣𠃛"), "n", "⿰𠁣𠃛"),
    new ElementType("25112511", new Cluster("門"), "n", "門"),
    new ElementType("25112511", new Cluster("⾨"), "n", "⾨"),
    //overlap point: Junda   tzai tzai 曙 3429 -- 閂 5040 //⿰𠁣𠃛  251125111
    new ElementType("(12|21)11254444", new Cluster("⿹⑥灬"), "o", "⿹⑥灬"),
    new ElementType("(12|21)11254444", new Cluster("馬"), "o", "馬"),
    new ElementType("(12|21)11254444", new Cluster("⾺"), "o", "⾺"),
    //overlap point: Junda   tzai  驤 4204 (12|21)1125444441251251112213534  ⿹⑥灬

    new ElementType("34(1|4)(51154|511211)", new Cluster("⿱人⿱丶⑤"), "p", "⿱人⿱丶⑤"),
    new ElementType("34(1|4)(51154|511211)", new Cluster("食"), "p", "食"),
    new ElementType("34(1|4)(51154|511211)", new Cluster("飠"), "p", "飠"),
    new ElementType("34(1|4)(51154|511211)", new Cluster("⾷"), "p", "⾷"),
    new ElementType("34(1|4)(51154|511211)", new Cluster("⻝"), "p", "⻝"),
    new ElementType("34(1|4)(51154|511211)", new Cluster("⻞"), "p", "⻞"),
    new ElementType("34(1|4)(51154|511211)", new Cluster("⻟"), "p", "⻟"),
    //overlap point: Junda   tzai 餞 4285  34(1|4)(51154|511211)(1534|1543)\3
    new ElementType("1251112", new Cluster("車"), "q", "車"),
    new ElementType("1251112", new Cluster("⾞"), "q", "⾞"),
  */


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

    val testres2 = ConwayToOutput.rawConwayToOutputCodes(Grapheme.splitIntoGraphemes("臒").map(x => Grapheme(x)).toList)

    testres2 shouldBe Set(
      "ptns", "ptjs", "ptnjus", "ptjvcs", "ptjjus", "ptjnus",
      "phns", "phjs", "phnjus", "phjvcs", "phjjus", "phjnus" )

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
