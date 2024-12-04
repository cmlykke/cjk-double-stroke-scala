package OutputTranslation

import UtilityClasses.{Grapheme, OutputEntry, OutputEntryFrequency}
import UtilityClasses.OutputEntryFrequency.Junda
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import staticFileGenerators.JundaFrequency.GenerateJundaMap
import staticFileGenerators.TzaiFrequency.GenerateTzaiMap

class OutputOverlapTest extends AnyFlatSpec with Matchers {


  it should "check the frequency of Junda 4-code characters above the first 9" in {
    val jundaFour = OutputOverlapObjects.singleJundaAboveNineFourCode //BLCUJundaAboveNineThreeCode

    val mapper = new GenerateJundaMap()
    val jundaMap = mapper.getJundaMap()

    val setOfChars: Set[String] = jundaFour
      .map(eachTupple => Grapheme.splitIntoGraphemes(eachTupple._3)
        .filter(graph => jundaMap.contains(graph))).flatten.toSet
    val frequencies: Double = setOfChars.filter(anyChar => jundaMap.contains(anyChar))
      .map(jundaChar => jundaMap.get(jundaChar).get)
      .map(jundaObj => jundaObj.frequency).sum

    val times100000 = frequencies * 100000

    val expected = 8.53E-6
    val tolerance = 0.01E-6
    frequencies should be(expected +- tolerance)

    setOfChars.size shouldBe 246
  }

  it should "check the frequency of Tzai 4-code characters above the first 9" in {
    val tzaiFour = OutputOverlapObjects.singleTzaiAboveNineFourCode //BLCUJundaAboveNineThreeCode

    val mapper = new GenerateTzaiMap()
    val tzaiMap = mapper.getTzaiMap()

    val setOfChars: Set[String] = tzaiFour
      .map(eachTupple => Grapheme.splitIntoGraphemes(eachTupple._3)
        .filter(graph => tzaiMap.contains(graph))).flatten.toSet
    val frequencies: Double = setOfChars.filter(anyChar => tzaiMap.contains(anyChar))
      .map(jundaChar => tzaiMap.get(jundaChar).get)
      .map(jundaObj => jundaObj.frequency).sum

    val times200000 = frequencies * 200000

    val expected = 4.16E-6
    val tolerance = 0.01E-6
    frequencies should be(expected +- tolerance)

    setOfChars.size shouldBe 416
  }

  it should "check the frequency of Tzai 6-code characters above the first 9" in {
    val tzaiSix = OutputOverlapObjects.singleTzaiAboveNineSixCode //BLCUJundaAboveNineThreeCode

    val mapper = new GenerateTzaiMap()
    val tzaiMap = mapper.getTzaiMap()

    val setOfChars: Set[String] = tzaiSix
      .map(eachTupple => Grapheme.splitIntoGraphemes(eachTupple._3)
        .filter(graph => tzaiMap.contains(graph))).flatten.toSet
    val frequencies: Double = setOfChars.filter(anyChar => tzaiMap.contains(anyChar))
      .map(jundaChar => tzaiMap.get(jundaChar).get)
      .map(jundaObj => jundaObj.frequency).sum

    val times400000 = frequencies * 400000

    val expected = 2.54E-6
    val tolerance = 0.01E-6
    frequencies should be(expected +- tolerance)
    setOfChars.size shouldBe 125
  }

  it should "verify the junda characters beyond nine - single characters" in {
    val jundaFour = OutputOverlapObjects.singleJundaAboveNineFourCode //BLCUJundaAboveNineThreeCode
    val jundaSix = OutputOverlapObjects.singleJundaAboveNineSixCode

    val firstTest: String = OutputOverlapObjects.singleStrFromList(jundaFour.take(10))
    val secondTest: String = OutputOverlapObjects.singleStrFromList(jundaSix.take(10))

    val testSix = OutputOverlapObjects.jundaAboveNine.filter(y => y._1.size == 6)

    val xhjo = OutputSorting.mapFullJunda.get("xhjo")
    val jnho = OutputSorting.mapFullJunda.get("jnho")
    val kxho = OutputSorting.mapFullJunda.get("kxho")
    val ejoo = OutputSorting.mapFullJunda.get("ejoo")
    val yhso = OutputSorting.mapFullJunda.get("yhso")

    val jhxwwo = OutputSorting.mapFullJunda.get("jhxwwo")
    val nhxwwo = OutputSorting.mapFullJunda.get("nhxwwo")
    val ohcyjo = OutputSorting.mapFullJunda.get("ohcyjo")
    val hhxjgo = OutputSorting.mapFullJunda.get("hhxjgo")
    val jhxwwg = OutputSorting.mapFullJunda.get("jhxwwg")

    val res1 = OutputOverlapObjects.outputElevenEntryNestedListToString(
      List("xhjo", "jnho", "kxho", "ejoo", "yhso"),
      List(xhjo, jnho, kxho, ejoo, yhso), OutputEntryFrequency.Junda)

    res1.replaceAll("\\s", "") shouldBe
      """
xhjo 是 3, 果 165, 题 218, 味 844, 暴 1028, 颗 1448, 啧 3698, 嗷 4104, 杲 4837, 題 5105
jnho 其 85, 联 356, 某 517, 职 616, 聚 1306, 欺 1699, 耿 2687, 戡 4817, 颞 5037, 聩 5209
kxho 真 204, 硬 1170, 颠 2134, 硕 2304, 碟 2835, 碱 2872, 磺 3547, 戛 4503, 碛 4790, 砹 5772
ejoo 数 231, 类 311, 粮 1303, 粪 2884, 糠 4169, 類 4915, 粽 5076, 粳 5458, 糅 5544, 糇 5898
yhso 镶 2974, 镰 3824, 镁 4023, 锭 4199, 铰 4630, 铱 4787, 锒 5340, 镧 5547, 镞 5762, 锿 5941""".replaceAll("\\s", "")

    firstTest.replaceAll("\\s", "") shouldBe """
xhjo 5105 題 5105, 嗉 5351, 顆 7901, 煚 0, 嘖 0, 暪 0, 昺 0, 曚 0, 暵 0, 晸 0
jnho 5209 聩 5209, 綦 5599, 葜 6009, 蓁 6630, 職 7201, 褧 7565, 顳 8192, 聧 8288, 聺 8395, 聝 8564
kxho 5772 砹 5772, 硖 5856, 碶 6293, 礎 6546, 礙 6767, 礞 7088, 頁 7491, 磦 8317, 矙 8384, 磩 8524
ejoo 5898 糇 5898, 敉 6086, 糞 6747, 粎 6785, 糷 7630, 糢 7918, 粷 8121, 粶 8607, 粻 8774, 粖 9084
yhso 5941 锿 5941, 镓 6079, 锬 6189, 镔 6399, 钬 6713, 镟 6729, 镲 7557, 镦 7920, 𬭯 0
yhds 6247 锾 6247, 铥 7445, 镵 8066, 钑 8767, 𬬮 0
pxjh 6310 鲭 6310, 鲳 6372, 鮊 6549, 鲴 6565, 鯝 7144, 鲊 7344, 鯡 7717, 鳣 7781, 鮨 7927, 鱣 7928
pxjo 6451 鳜 6451, 鱇 6484, 鲽 6555, 鲦 6606, 鳈 6667, 鮡 6844, 鰜 6922, 鯮 6926, 鳀 6931, 鱵 7119
yhgo 6536 铗 6536, 铼 7005, 锳 7957, 𫟹 0, 𫓹 0, 𫓧 0, 𨱑 0, 䦄 0
jjko 6578 趱 6578, 葴 7311, 趩 7351, 趧 7680, 薾 7820, 趮 8170, 莢 8628, 蒧 8893, 赽 9285, 趪 9570""".replaceAll("\\s", "")

    secondTest.replaceAll("\\s", "") shouldBe """
pxjlwo 7522 鯕 7522, 鰷 8250, 鯁 8519, 魜 8733, 鯀 9014, 鱮 9059, 魰 9668, 鯐 9673, 鰬 9678, 鱌 9682
jhxwwo 8511 駃 8511, 騠 8513, 騛 8552, 駷 8727, 騄 9186, 駣 9650, 駥 9651, 驖 9660, 驥 9661, 騍 9789
nhxwwo 8511 駃 8511, 騠 8513, 騛 8552, 駷 8727, 騄 9186, 駣 9650, 駥 9651, 驖 9660, 驥 9661, 騍 9789
ohcyjo 9307 鎍 9307, 鐷 9736, 鍱 9913, 錸 0, 鏌 0, 錤 0, 鎱 0, 鋉 0, 鏾 0, 鐼 0
jhmeio 9666 鬢 9666, 鬒 0, 鬕 0, 鬗 0, 鬞 0, 鬤 0, 髸 0, 䰖 0, 䰒 0, 䰏 0
nhmeio 9666 鬢 9666, 鬒 0, 鬕 0, 鬗 0, 鬞 0, 鬤 0, 髸 0, 䰖 0, 䰒 0, 䰏 0
pxjlwh 9675 鯖 9675, 魽 9920, 鮪 9926, 魯 0, 鱷 0, 鱈 0, 鰭 0, 鯧 0, 鰗 0, 鰆 0
pxjlwn 9675 鯖 9675, 鮭 9848, 鯉 0, 鱸 0, 鱹 0, 鱕 0, 鯥 0, 鯭 0, 鱃 0, 鰛 0
pxjlws 9925 鮘 9925, 鰱 0, 魷 0, 鱁 0, 魬 0, 魼 0, 魡 0, 鯪 0, 鰒 0, 鯫 0
gzzzzz 2147483647 ： 0, ， 0, 𡿨 0, 〻 0, 、 0, ⺄ 0, ⺃ 0, ⺂ 0""".replaceAll("\\s", "")

    jundaFour.length shouldBe 464
    jundaSix.length shouldBe 102
    val test22 = ""
  }

  it should "verify the tzai characters beyond nine - single characters" in {
    val tzaiFour = OutputOverlapObjects.singleTzaiAboveNineFourCode
    val tzaiSix = OutputOverlapObjects.singleTzaiAboveNineSixCode

    val firstTest: String = OutputOverlapObjects.singleStrFromList(tzaiFour.take(10))
    val secondTest: String = OutputOverlapObjects.singleStrFromList(tzaiSix.take(10))

    val jnxo = OutputSorting.mapFullTzai.get("jnxo")
    val kxho = OutputSorting.mapFullTzai.get("kxho")
    val xjgo = OutputSorting.mapFullTzai.get("xjgo")
    val xhjo = OutputSorting.mapFullTzai.get("xhjo")
    val wjgo = OutputSorting.mapFullTzai.get("wjgo")

    val jjxo = OutputSorting.mapFullTzai.get("jjxo")

    val jhxwwo = OutputSorting.mapFullTzai.get("jhxwwo")
    val nhxwwo = OutputSorting.mapFullTzai.get("nhxwwo")
    val ohcyxo = OutputSorting.mapFullTzai.get("ohcyxo")
    val ohcyjo = OutputSorting.mapFullTzai.get("ohcyjo")
    val ohcyko = OutputSorting.mapFullTzai.get("ohcyko")

    val res1 = OutputOverlapObjects.outputElevenEntryNestedListToString(
      List("jnxo", "kxho", "xjgo", "xhjo", "wjgo"),
      List(jnxo, kxho, xjgo, xhjo, wjgo), OutputEntryFrequency.Tzai)

    res1.replaceAll("\\s", "") shouldBe
      """
jnxo 歡 246, 散 1009, 鞭 2440, 艱 2587, 歎 2649, 蔑 3192, 鞠 3354, 鞦 4394, 顢 5123, 鞅 5412
kxho 真 95, 碟 531, 硬 593, 碩 1100, 頁 1309, 礙 1717, 礎 1782, 顛 2354, 磺 4346, 饜 5477
xjgo 戰 331, 員 345, 興 357, 嚴 805, 喂 1946, 噪 2531, 唄 4185, 顎 4742, 噥 5278, 嘳 5724
xhjo 是 2, 果 99, 題 105, 味 709, 暴 871, 顆 1051, 煚 3373, 嘖 3684, 嗷 4578, 暪 5811
wjgo 濃 1726, 澡 2341, 燙 2720, 潰 2738, 瀑 2826, 瀾 3176, 濺 4035, 泱 4334, 灝 5124, 漯 5821""".replaceAll("\\s", "")

    firstTest.replaceAll("\\s", "") shouldBe
      """
jnxo 5412 鞅 5412, 顴 5454, 茦 5879, 蔌 6853, 蓛 8187, 蕀 8641, 鞣 8818, 韇 9286, 靺 9664, 蔈 10379
kxho 5477 饜 5477, 戛 5769, 礞 6560, 硤 7317, 矙 7439, 磌 8205, 礣 8305, 磧 8356, 礥 8733, 睼 9636
xjgo 5724 嘳 5724, 囅 8325, 辴 8725, 哫 8887, 爨 9314, 戙 10087, 嚗 10482, 喿 10901, 爂 12337, 颚 0
xhjo 5811 暪 5811, 杲 6045, 嗉 6229, 昺 6787, 曚 6866, 暵 7865, 晸 8142, 嚽 9257, 暕 10909, 褁 12078
wjgo 5821 漯 5821, 灦 6175, 淏 6807, 湜 6969, 澖 6992, 浿 7744, 淟 8463, 渨 8974, 潩 9689, 溳 10262
wjgh 6234 澠 6234, 淐 7300, 灗 7670, 湒 8124, 駏 9105, 泪 0, 濐 0, 㶄 0, 㵎 0, 㴘 0
jjjh 6434 莤 6434, 蓍 6845, 蘁 7032, 葙 7333, 荁 8917, 蘛 9244, 芏 9339, 躤 12759, 苷 0, 蓸 0
pxjo 6470 鯕 6470, 鰷 6649, 鱌 6651, 鱳 6887, 鯀 7234, 鰬 7425, 鮢 7917, 鮇 8654, 鮡 8682, 魰 9111
djgo 6856 嬠 6856, 梀 9445, 娖 10745, 姎 11347, 嬇 11619, 婰 11995, 棫 12050, 孍 12752, 娱 0, 娲 0
jjxo 6869 藈 6869, 虆 7042, 菋 8514, 蔂 8612, 躉 8819, 葨 9016, 蹎 9816, 薎 11137, 蔝 11638, 蕺 12296""".replaceAll("\\s", "")

    secondTest.replaceAll("\\s", "") shouldBe
      """
jhxwwo 5168 騾 5168, 驃 5281, 騄 5405, 騬 6128, 騋 6637, 騃 7017, 騵 7696, 騥 7948, 騠 7949, 騤 8298
nhxwwo 5168 騾 5168, 驃 5281, 騄 5405, 騬 6128, 騋 6637, 騃 7017, 騵 7696, 騥 7948, 騠 7949, 騤 8298
pxjlwo 6470 鯕 6470, 鰷 6649, 鱌 6651, 鱳 6887, 鯀 7234, 鰬 7425, 鮢 7917, 鮇 8654, 鮡 8682, 魰 9111
pxjlwh 7959 鰆 7959, 鱠 8339, 鮨 8681, 鱣 8791, 鮓 9782, 鮹 9860, 鱋 9943, 鰼 10592, 鰽 10593, 鯡 11219
ohcyxo 9186 鍡 9186, 鐰 9262, 錪 10444, 鑤 11313, 鉠 11561, 鉂 0, 鉙 0, 淾 0, 鐛 0, 鋜 0
ohcyjo 9214 鎍 9214, 鎱 9215, 鐷 9263, 鋉 9723, 鏾 11238, 鐼 11257, 鑶 11299, 鑟 0, 鏉 0, 鐄 0
ohcyko 9662 鋮 9662, 銊 12924, 鐝 0, 龯 0, 鏯 0
pxjlwg 9823 鮦 9823, 鱎 10600, 魡 11006, 鮥 11152, 鯦 11217, 鮐 11696, 鰝 11828, 鮚 12312, 鮶 12703, 鮎 0
jhxwwj 10557 騲 10557, 驔 10591, 馯 11563, 駍 11653, 驌 12751, 騈 0, 騨 0, 馸 0, 䮺 0, 䮨 0
nhxwwj 10557 騲 10557, 驔 10591, 馯 11563, 駍 11653, 驌 12751, 騈 0, 騨 0, 馸 0, 䮺 0, 䮨 0""".replaceAll("\\s", "")

    tzaiFour.length shouldBe 464
    tzaiSix.length shouldBe 102
    val test22 = ""
  }

  it should "check junda BCLU" in {
    val BCLUthree = OutputOverlapObjects.BLCUjundaAboveNineThreeCode //BLCUJundaAboveNineThreeCode
    val BCLUfive = OutputOverlapObjects.BLCUJundaAboveNineFiveCode

    val allThreeWordsUnder5000 = OutputOverlapObjects.jundaAboveNine.filter(y => y._1.size <= 3).map(x => x._2).flatten
      .toList.map(z => {
        val ordinal: Int = if (z.BCLUoptionBD.isDefined) z.BCLUoptionBD.get.ordinal else 9999999
        (ordinal, z.chineseStr)
      }).toList.sortBy(x => (x._1, x._2))

    val firstTest: String = OutputOverlapObjects.singleStrFromList(BCLUthree.take(10))
    val secondTest: String = OutputOverlapObjects.singleStrFromList(BCLUfive.take(10))

    val jox = OutputSorting.mapFullJunda.get("xhx")
    val joj = OutputSorting.mapFullJunda.get("jow")
    val uou = OutputSorting.mapFullJunda.get("kow")
    val kow = OutputSorting.mapFullJunda.get("uou")
    val jow = OutputSorting.mapFullJunda.get("tow")

    val jjjjj = OutputSorting.mapFullJunda.get("jjjjj")
    val jjjjg = OutputSorting.mapFullJunda.get("jjjjg")
    val dofhz = OutputSorting.mapFullJunda.get("dofhz")
    val hhjjo = OutputSorting.mapFullJunda.get("hhjjo")
    val wowkt = OutputSorting.mapFullJunda.get("wowkt")

    val res1 = OutputOverlapObjects.outputElevenEntryNestedListToString(
      List("jox", "joj", "uou", "kow", "jow"),
      List(jox, joj, uou, kow, jow), OutputEntryFrequency.BCLU)

    val res2 = OutputOverlapObjects.outputElevenEntryNestedListToString(
      List("jjjjj", "jjjjg", "dofhz", "wertn", "wowkt"),
      List(jjjjj, jjjjg, dofhz, hhjjo, wowkt), OutputEntryFrequency.BCLU)

    res1.replaceAll("\\s", "") shouldBe
      """
jox 明显 484, 昨日 696, 明星 1711, 昨晚 1955, 明明 3466, 口味 4610, 回收 5110, 回国 5540, 口号 5579, 贿赂 6639
joj 其实 337, 政治 708, 教学 1074, 真实 1550, 联赛 1634, 其它 1721, 荣誉 4518, 震惊 4963, 教室 5233, 黄河 5702
uou 感觉 356, 欧洲 1283, 感情 1328, 真实 1550, 感染 1949, 顾客 2966, 眼泪 2973, 原油 3493, 龙头 4595, 感慨 5567
kow 价 545, 你们 210, 休息 1686, 集体 1707, 货币 2417, 伙伴 3302, 似的 3547, 倾向 4065, 保健 4273, 促使 4676
jow 决定 394, 资源 592, 旅游 845, 交流 1201, 决赛 2211, 文学 2474, 文字 2744, 旅客 3135, 病情 4238, 亲密 6260""".replaceAll("\\s", "")

    firstTest.replaceAll("\\s", "") shouldBe """
jox 3153 歌曲 3153, 或是 3382, 救助 4101, 跌幅 6023, 卖出 6254, 茶叶 6663, 跳水 8434, 职员 9432, 歌唱 9647, 跳出 9836
joj 4410 两者 4410, 跟踪 5054, 走路 6086, 蒙古 7103, 荣获 7632, 菊花 7688, 歌声 7744, 栽培 8108, 职场 8686, 芙蓉 9614
uou 4676 促使 4676, 顺便 6106, 保佑 7330, 保住 10366, 供货 10676, 休假 10924, 保修 15699, 倾倒 17729, 做作 17788, 供血 22744
kow 5567 感慨 5567, 感激 5985, 感性 8576, 真情 8787, 原定 9512, 真空 10145, 感悟 10666, 爽快 15140, 碱性 16131, 感官 17541
jow 5702 黄河 5702, 救灾 7912, 救治 8230, 真情 8787, 政法 8843, 救济 9792, 真空 10145, 裁定 10907, 更深 11707, 赤字 12322
wot 6066 深夜 6066, 额度 7225, 头疼 7321, 演变 7829, 溃疡 9180, 实效 12267, 实况 17678, 泳装 19138, 淡忘 19600, 家畜 21953
tow 6260 亲密 6260, 资深 7199, 亲情 7666, 放学 9925, 放宽 10390, 交涉 12855, 交割 13525, 放慢 14026, 床头 14897, 装满 15097
kox 6381 感叹 6381, 眼圈 10738, 成败 12712, 硬是 13465, 眼见 13939, 眼影 15592, 眼界 17864, 成因 19176, 残暴 23375, 残骸 23446
xhx 6639 贿赂 6639, 明日 7875, 口岸 8186, 口水 8232, 骨髓 9193, 暗中 9391, 回回 9661, 胃口 9696, 日圆 9893, 暗暗 10554
woj 6875 实惠 6875, 快要 7359, 头顶 8248, 兴起 8428, 实地 8936, 实事 9163, 演艺 9560, 激起 10212, 惊醒 11096, 定下 12354""".replaceAll("\\s", "")

    res2.replaceAll("\\s", "") shouldBe
      """
jjjjj 劳斯莱斯 30400, 塔吉克斯坦 34769, 雷克萨斯 50495, 斯芬克斯 198522, 萬事起頭難 0, 華茲華斯 0, 雷克薩斯 0, 埋頭苦幹 0, 聖地亞哥 0, 草草 16241
jjjjg 走下坡路 36798, 支支吾吾 43402, 刺芹菇 0, 薄荷酮 0, 聖荷西 0, 跌跌蹌蹌 0, 歡歡喜喜 0, 亞達薛西 0, 可喜 13017, 赶超 21998
dofhz 妹子 5780, 猴子 8730, 架子 12887, 娘子 14322, 桃子 20133, 棋子 21031, 林子 22657, 婊子 23016, 模子 35061, 榛子 41056
wertn 三三两两 35092, 一神教 0, 禍福與共 0, 三三兩兩 0, 三天兩頭 0, 青藏 10735, 青菜 13849, 甜菜 33118, 三藏 45860, 春茶 50231
wowkt 源头 8263, 额头 9360, 滨海 9433, 深海 19271, 头头 31101, 派头 37204, 浅海 49270, 定海 50955, 浪头 53058, 兴头 58266""".replaceAll("\\s", "")

    secondTest.replaceAll("\\s", "") shouldBe """
jjjjj 16241 草草 16241, 草莽 60224, 莽草 116443, 莘莘 0, 葎草 0, 南華 0, 薺苧 0, 薺薴 0
jjjjg 21998 赶超 21998, 可嘉 41991, 草菇 55018, 幸喜 101140, 革吉 325548, 藓苔 0, 趕超 0, 幹警 0, 蘚苔 0
dofhz 41056 榛子 41056, 核子 54143, 禁卫 59669, 楔子 77100, 椽子 96363, 戳子 197696, 柰子 422961, 橡子 0, 槟子 0, 樣子 0
hhjjo 50231 春茶 50231, 青蒜 81400, 三芝 0, 甜菊 0, 春藥 0, 三蘇 0
wowkt 58266 兴头 58266, 兴海 85472, 穴头 153122, 案头 0, 濒海 0, 溟海 0, 憷头 0, 淺海 0, 濱海 0, 瀕海 0
joxhn 65064 艾叶 65064, 丙申 65480, 某甲 74352, 联星 0, 頭里 0, 東盟 0, 歐盟 0
wertf 72309 滨州市 72309, 池州市 77437, 汝州市 82592, 定州市 93725, 涿州市 102823, 深州市 159548, 清州市 565366, 溫州市 0, 滄州市 0, 濱州市 0
jnjjn 84655 基址 84655, 蕴蓄 113394, 雷蓋 0, 盐埕 0, 鹽埕 0, 基臺 0, 蘿蔔 0, 蘊蓄 0, 茳芏 0
jojjo 90189 茱萸 90189, 欺蒙 142954, 敬茶 0, 贡茶 0, 藤菜 0, 戴菊 0, 蒙藥 0, 黃菊 0, 蹀蹀 0, 貢茶 0
jkjxo 92884 芦荻 92884""".replaceAll("\\s", "")

    BCLUthree.length shouldBe 3813
    BCLUfive.length shouldBe 330
    val test22 = ""
  }


  it should "check tzai Sinica" in {
    val SINICAthree = OutputOverlapObjects.sinicaTzaiAboveNineThreeCode //BLCUJundaAboveNineThreeCode
    val SINICAfive = OutputOverlapObjects.sinicaTzaiAboveNineFiveCode

    val allThreeWordsUnder5000 = OutputOverlapObjects.tzaiAboveNine.filter(y => y._1.size <= 3).map(x => x._2).flatten
      .toList.map(z => {
        val ordinal: Int = if (z.sinicaOptionSD.isDefined) z.sinicaOptionSD.get.ordinal else 9999999
        (ordinal, z.chineseStr)
      }).toList.sortBy(x => (x._1, x._2))

    val firstTest: String = OutputOverlapObjects.singleStrFromList(SINICAthree.take(10))
    val secondTest: String = OutputOverlapObjects.singleStrFromList(SINICAfive.take(10))

    val uox = OutputSorting.mapFullTzai.get("uox")
    val joj = OutputSorting.mapFullTzai.get("joj")
    val jou = OutputSorting.mapFullTzai.get("jou")
    val jot = OutputSorting.mapFullTzai.get("jot")
    val uou = OutputSorting.mapFullTzai.get("uou")

    val dnfhz = OutputSorting.mapFullTzai.get("dhfhz")
    val dodzz = OutputSorting.mapFullTzai.get("dnfhz")
    val dofhz = OutputSorting.mapFullTzai.get("dodzz")
    val dojgo = OutputSorting.mapFullTzai.get("dofhz")
    val dojho = OutputSorting.mapFullTzai.get("dojho")

    val res1 = OutputOverlapObjects.outputElevenEntryNestedListToString(
      List("uox", "joj", "jou", "jot", "uou"),
      List(uox, joj, jou, jot, uou), OutputEntryFrequency.Sinica)

    val res2 = OutputOverlapObjects.outputElevenEntryNestedListToString(
      List("dnfhz", "dodzz", "dofhz", "dojgo", "dojho"),
      List(dnfhz, dodzz, dofhz, dojgo, dojho), OutputEntryFrequency.Sinica)

    res1.replaceAll("\\s", "") shouldBe
      """
uox 集團 1431, 集中 1444, 樂團 1981, 樂器 2198, 依照 2317, 傑出 2491, 做出 3006, 便是 3144, 樂園 3284, 儀器 3308
joj 東西 310, 或者 519, 故事 567, 真正 687, 蘇聯 885, 頭髮 2517, 歌聲 3569, 走路 3586, 執著 3618, 茶葉 3864
jou 其他 177, 真的 452, 教師 751, 教練 1646, 走向 2416, 走進 2657, 聚集 3372, 城堡 3400, 歡樂 3652, 職位 4046
jot 政府 122, 教育 180, 英文 1488, 其次 1908, 東方 2101, 東京 2256, 故意 2526, 更新 2537, 東部 3522, 頭痛 4093
uou 你們 298, 價值 505, 條件 536, 促進 1532, 休息 1691, 條例 2228, 促使 2422, 傾向 3049, 價位 3605, 線條 4327""".replaceAll("\\s", "")

    firstTest.replaceAll("\\s", "") shouldBe """
uox 3308 儀器 3308, 樂曲 5267, 集體 5445, 供水 6827, 使出 7378, 貨品 7437, 係數 7868, 樂山 0, 便中 0, 保山 0
joj 3864 茶葉 3864, 走勢 3908, 東亞 4078, 長達 4218, 執政 4381, 歡喜 4567, 緊緊 4865, 與其 5002, 東歐 5139, 長遠 5758
jou 4046 職位 4046, 政經 6272, 束縛 6991, 教化 7707, 票價 7903, 跟進 8146, 真個 0, 真經 0, 長進 0, 東經 0
jot 4093 頭痛 4093, 長度 4374, 共享 5176, 頭部 6959, 長庚 7254, 共產 8048, 散文 8651, 兩旁 8819, 執意 8864, 走廊 8996
uou 4327 線條 4327, 順便 4897, 綠島 5260, 供給 5424, 條約 5609, 候鳥 5689, 保健 6057, 休假 6689, 傢伙 6712, 綠化 6958
jsu 4591 連結 4591, 連絡 5795, 零件 5868, 邁進 5944, 零售 6562, 蔓延 7536, 去信 0, 報信 0, 取信 0, 取經 0
toj 4608 痕跡 4608, 變革 5213, 麻醉 5607, 文藝 5637, 廣東 5735, 放鬆 5926, 旗下 6265, 次長 7115, 京都 8314, 文革 8947
jox 4734 職員 4734, 蘋果 4913, 藥品 5555, 雨水 6049, 歌唱 6851, 跳水 7259, 共鳴 7298, 賣出 8085, 歌星 9511, 真是 0
jjw 4758 聯賽 4758, 南洋 8458, 華容 0, 華安 0, 可決 0, 南定 0, 南安 0, 南海 0, 可怪 0, 舉家 0
xoj 4851 農場 4851, 員警 5680, 買賣 5725, 果真 6068, 暴露 6079, 戰事 6081, 農地 6569, 收取 7264, 買下 7458, 喚起 7555""".replaceAll("\\s", "")

    res2.replaceAll("\\s", "") shouldBe
      """
dnfhz 女子 1665, 麵子 0, 猶子 0, 棚子 0, 柜子 0, 楦子 0, 猸子 0, 档子 0, 女卫 0
dodzz 檔子 0, 李子 0, 娃子 0, 柱子 0, 狸子 0, 柵子 0, 檯子 0, 柚子 0, 姪子 0, 栓子 0
dofhz 林木 0, 飛刀 0, 橫木 0, 槓刀 0, 橡木 0, 戮力 0, 槭木 0, 棶木 0, 横木 0, 枫木 0
dojgo 樣子 1194, 猴子 6474, 核子 7248, 機子 0, 林子 0, 妹子 0, 模子 0, 架子 0, 娘子 0, 棋子 0
dojho 好半天 0, 校長 744, 機長 0, 來項 0, 負載 0, 桃城 0, 翼城 0, 狹長 0, 檳城 0, 根鬚 0""".replaceAll("\\s", "")
    
    secondTest.replaceAll("\\s", "") shouldBe """
dnfhz 2147483647 栓子 0, 嬸子 0, 杠子 0
dodzz 2147483647 枫木 0, 梾木 0
dofhz 2147483647 棋子 0, 桃子 0, 檳子 0, 槓子 0, 戳子 0, 橡子 0, 婊子 0, 楔子 0, 榛子 0, 柰子 0
dojgo 2147483647 狡赖 0
dojho 2147483647 根鬚 0, 槟城 0, 杂项 0
dojjo 2147483647 林藪 0, 榛藪 0, 林茨 0, 横越 0
dowgo 2147483647 标定 0, 禁赛 0, 棋赛 0
doxho 2147483647 賀縣 0, 棋具 0, 頗具 0, 穎果 0, 榛果 0, 横是 0, 颖果 0, 颇具 0, 横暴 0, 标题 0
dsfhz 2147483647 嫚子 0, 犹子 0
dyhgj 2147483647 加利福尼亚大学洛杉矶分校 0""".replaceAll("\\s", "")
    
    SINICAthree.length shouldBe 3813
    SINICAfive.length shouldBe 330
    val test22 = ""
  }

  it should "check tzai BCLU" in {
    val BCLUthree = OutputOverlapObjects.BLCUTzaiAboveNineThreeCode //BLCUJundaAboveNineThreeCode
    val BCLUfive = OutputOverlapObjects.BLCUTzaiAboveNineFiveCode

    val allThreeWordsUnder5000 = OutputOverlapObjects.tzaiAboveNine.filter(y => y._1.size <= 3).map(x => x._2).flatten
      .toList.map(z => {
        val ordinal: Int = if (z.BCLUoptionBD.isDefined) z.BCLUoptionBD.get.ordinal else 9999999
        (ordinal, z.chineseStr)
      }).toList.sortBy(x => (x._1, x._2))

    val firstTest: String = OutputOverlapObjects.singleStrFromList(BCLUthree.take(10))
    val secondTest: String = OutputOverlapObjects.singleStrFromList(BCLUfive.take(10))

    val cht = OutputSorting.mapFullTzai.get("cht")
    val kee = OutputSorting.mapFullTzai.get("kee")
    val mdw = OutputSorting.mapFullTzai.get("mdw")
    val ffl = OutputSorting.mapFullTzai.get("ffl")
    val obt = OutputSorting.mapFullTzai.get("obt")

    val towho = OutputSorting.mapFullTzai.get("towho")
    val jhtpo = OutputSorting.mapFullTzai.get("jhtpo")
    val towgo = OutputSorting.mapFullTzai.get("towgo")
    val poxho = OutputSorting.mapFullTzai.get("poxho")
    val johns = OutputSorting.mapFullTzai.get("johns")

    val res1 = OutputOverlapObjects.outputElevenEntryNestedListToString(
      List("cht", "kee", "mdw", "ffl", "obt"),
      List(cht, kee, mdw, ffl, obt), OutputEntryFrequency.BCLU)

    val res2 = OutputOverlapObjects.outputElevenEntryNestedListToString(
      List("towho", "jhtpo", "towgo", "poxho", "johns"),
      List(towho, jhtpo, towgo, poxho, johns), OutputEntryFrequency.BCLU)
    
    res1.replaceAll("\\s", "") shouldBe
      """
cht 情意 21231, 情資 0, 情變 0, 情商 0, 情敵 0, 情癡 0, 情痴 78927, 怕癢 0, 怕痒 108120, 情况 159
kee 成為 0, 成道 409733, 成精 0, 殘卷 0, 砂輪 0, 砂糖 30403, 殲擊 0, 戊糖 140409, 殘羹 0, 成为 180
mdw 比賽 0, 比容 98754, 比安 555614, 切實 0, 切激 0, 房室 28791, 房客 25389, 切割 12729, 比濕 0, 比赛 220
ffl 出手 4237, 節電 0, 出招 0, 節拍 0, 降雪 23793, 出操 61658, 節操 0, 出露 52598, 函授 28996, 出来 313
obt 創意 0, 創新 0, 創立 0, 創辦 0, 創痛 0, 劍龍 0, 劍麻 0, 創痕 0, 剉冰 0, 创新 748""".replaceAll("\\s", "")

    firstTest.replaceAll("\\s", "") shouldBe """
cht 159 情况 159, 情状 56117, 情变 0, 情资 0, 当夜 35022, 阎魔 0, 间充 0
kee 180 成为 180, 残卷 0, 残羹 0
mdw 220 比赛 220, 比湿 173709, 轮空 36561, 轮流 11135, 切实 2826, 轮滑 0, 轮渡 23726
ffl 313 出来 313
obt 748 创新 748, 创意 2872, 创立 9289, 创痛 77158, 剑麻 82327, 判决 3769, 创痕 143391
qgn 792 讯 792
osr 866 爱情 866, 公愤 57708, 令阃 0, 爱惜 19575, 爱憎 67186, 爱怜 43567, 会门 82764, 爱问 0
jfo 988 都会 988, 聊叙 0, 节食 23531, 韩愈 35834, 画卷 23645, 节余 42047, 带领 3134
roy 1141 恢复 1141, 快锅 348009, 快钱 0, 闪失 35836, 闲适 34938, 惊错 0, 闲钱 46032, 闪铄 404181
dhs 1142 相对 1142, 相劝 45224, 查对 58833""".replaceAll("\\s", "")

    res2.replaceAll("\\s", "") shouldBe
      """
towho 資源 0, 病源 50208, 廣漢 0, 族滅 0, 資淺 0, 廉潔 0, 廣漠 0, 衰減 0, 褻瀆 0, 资源 592
jhtpo 工資 0, 增資 0, 士族 0, 土族 69716, 朝族 0, 著衣 117761, 巨資 0, 垣衣 0, 赭衣 0, 工资 1561
towgo 六家 0, 病家 79562, 文宗 84049, 廣宗 0, 廠家 0, 哀家 0, 庚寅 63227, 亲家 31496, 决定 394, 决赛 2211
poxho 負數 0, 閃映 0, 頗具 0, 穎果 0, 滕縣 0, 颖果 213551, 颇具 0, 风味 10094, 风暴 6376, 风景 3277
johns 英式 0, 越式 0, 真武 280499, 長武 0, 英武 48683, 跟丟 0, 走丟 0, 蘇武 0, 歐式 0, 款式 6507""".replaceAll("\\s", "")
    
    secondTest.replaceAll("\\s", "") shouldBe """
towho 592 资源 592, 装潢 16524, 装满 15097, 亵渎 26872, 资浅 0
jhtpo 1561 工资 1561, 增资 12484, 巨资 15075
towgo 2211 决赛 2211
poxho 3277 风景 3277
johns 6507 款式 6507, 菜式 0, 臥式 0, 藜麦 0, 莜麦 132974
xjtej 8283 崭新 8283, 申辩 36279
jsozz 11917 过人 11917
jsnjo 12608 菠菜 12608, 芍藥 0
wojhe 15865 满载 15865
jwjtf 16637 南宁市 16637, 菏泽市 69149""".replaceAll("\\s", "")

    BCLUthree.length shouldBe 3813
    BCLUfive.length shouldBe 330
    val test = ""
  }


  it should "check junda Sinica" in {
    val SINICAthree = OutputOverlapObjects.sinicaJundaAboveNineThreeCode //BLCUJundaAboveNineThreeCode
    val SINICAfive = OutputOverlapObjects.sinicaJundaAboveNineFiveCode

    val allThreeWordsUnder5000 = OutputOverlapObjects.jundaAboveNine.filter(y => y._1.size <= 3).map(x => x._2).flatten
      .toList.map(z => {
        val ordinal: Int = if (z.sinicaOptionSD.isDefined) z.sinicaOptionSD.get.ordinal else 9999999
        (ordinal, z.chineseStr)
      }).toList.sortBy(x => (x._1, x._2))

    val firstTest: String = OutputOverlapObjects.singleStrFromList(SINICAthree.take(10))
    val secondTest: String = OutputOverlapObjects.singleStrFromList(SINICAfive.take(10))

    val xta = OutputSorting.mapFullJunda.get("xta")
    val ons = OutputSorting.mapFullJunda.get("ons")
    val xcu = OutputSorting.mapFullJunda.get("xcu")
    val ugx = OutputSorting.mapFullJunda.get("ugx")
    val oni = OutputSorting.mapFullJunda.get("oni")

    val joxjt = OutputSorting.mapFullJunda.get("joxjt")
    val ynfhz = OutputSorting.mapFullJunda.get("ynfhz")
    val uoxjg = OutputSorting.mapFullJunda.get("uoxjg")
    val jhtpo = OutputSorting.mapFullJunda.get("jhtpo")
    val toxho = OutputSorting.mapFullJunda.get("toxho")

    val res1 = OutputOverlapObjects.outputElevenEntryNestedListToString(
      List("xta", "ons", "xcu", "ugx", "oni"),
      List(xta, ons, xcu, ugx, oni), OutputEntryFrequency.Sinica)
    
    val res2 = OutputOverlapObjects.outputElevenEntryNestedListToString(
      List("joxjt", "ynfhz", "uoxjg", "jhtpo", "toxho"),
      List(joxjt, ynfhz, uoxjg, jhtpo, toxho), OutputEntryFrequency.Sinica)
    
    res1.replaceAll("\\s", "") shouldBe
      """
xta 国际 0, 图书 0, 国防 0, 图纸 0, 因缘 0, 国书 0, 国队 0, 啃书 0, 國防 3722, 國際 245
ons 全能 0, 坐台 0, 益虫 0, 坐蜡 0, 益蟲 0, 貓熊 0, 坐蠟 0, 雞鶩 0, 益發 0, 學習 319
xcu 时候 0, 时代 0, 时段 0, 吓倒 0, 时延 0, 时价 0, 时俗 0, 时任 0, 吓傻 0, 時代 323
ugx 信号 0, 估量 0, 向量 5933, 低贱 0, 低吟 0, 信口 0, 低昂 0, 儋县 0, 結出 0, 結果 375
oni 全盘 0, 番瓜 0, 全般 0, 雞雞 0, 全盤 0, 全託 0, 全復 0, 學徒 0, 學說 5003, 學術 568""".replaceAll("\\s", "")

    firstTest.replaceAll("\\s", "") shouldBe """
xta 245 國際 245, 團隊 2930, 國隊 0
ons 319 學習 319
xcu 323 時代 323, 時任 0, 時候 106, 時段 4678, 時延 0, 時俗 0, 時綏 0, 時價 0
ugx 375 結果 375, 結界 0, 結盟 8234, 結晶 8481, 結喉 0, 結點 0, 約出 0, 約同 0, 約略 0, 約旦 0
oni 568 學術 568, 學識 8887, 學銜 0
uwt 681 終於 681, 繃床 0, 烏亮 0, 偽迹 0, 鳥疫 0, 烏龍 0
ucu 785 傳統 785, 傳給 0
lom 815 找到 815, 找遍 0, 换妻 0, 操切 0, 拱肩 0, 攘夷 0, 揍扁 0, 擴充 3230, 換牙 0, 換妻 0
xyp 883 國外 883, 國名 0, 國關 0, 國風 0, 國門 0, 國腳 0, 國貿 0
upf 902 兒子 902, 化妝 6447, 兒孫 0""".replaceAll("\\s", "")

    res2.replaceAll("\\s", "") shouldBe
      """
joxjt 颧骨 0, 颞骨 0, 菜圃 0, 顶骨 0, 踝骨 0, 頭骨 0, 頂骨 0, 顳骨 0, 顴骨 0, 英國 908
ynfhz 罐子 0, 锥子 0, 壬子 0, 重子 0, 稚子 0, 镏子 0, 季子 0, 程子 0, 生子 0, 種子 1942
uoxjg 仪器 0, 兵器 0, 伙同 0, 货品 0, 便器 0, 供品 0, 織品 0, 貨品 7437, 樂器 2198, 儀器 3308
jhtpo 工资 0, 增资 0, 巨资 0, 土族 0, 著衣 0, 士族 0, 朝族 0, 垣衣 0, 赭衣 0, 工資 3808
toxho 效果 635, 放映 7788, 文具 0, 浆果 0, 京味 0, 韻味 0, 齋果 0, 文縣 0, 康縣 0, 次數 4002""".replaceAll("\\s", "")


    secondTest.replaceAll("\\s", "") shouldBe """
joxjt 908 英國 908, 越國 0, 故國 0, 故園 0, 索國 0, 救國 0, 菜園 0, 貢國 0, 賣國 0, 兩國 0
ynfhz 1942 種子 1942
uoxjg 3308 儀器 3308
jhtpo 3808 工資 3808, 增資 8886, 巨資 0
toxho 4002 次數 4002, 癡呆 0, 哀嘆 0, 變數 4703
wjoxo 6143 宜蘭縣 6143, 家政員 0, 馬赫數 0, 演藝人員 0, 宮城縣 0, 寧城縣 0, 寬城縣 0, 涼城縣 0, 寧蒗縣 0
jhnjo 6867 菁英 6867, 雪萊 0, 趙薇 0, 薔薇 0, 薯蕷 0, 薺菜 0
tojho 7115 次長 7115, 族長 0, 旅長 0, 廠長 0
toumo 7359 交貨 7359, 韻緻 0, 次貸 0, 放貸 0, 文牘 0
xfxho 7584 凸顯 7584""".replaceAll("\\s", "")

    SINICAthree.length shouldBe 3813
    SINICAfive.length shouldBe 330
    val test22 = ""
  }


  private def tuppleTwoToStr(input: List[(String, Int)]): String = {
    if (input.size > 9) {
      val res  = input.take(10).map(x =>  "(" + x._1 +", " + x._2 + ")").mkString("\r\n").trim
      return res
    } else if (input.size > 0) {
      val res = input.map(x => "(" + x._1 + ", " + x._2 + ")").mkString("\r\n").trim
      return res
    } else {
      return "None"
    }
  }
  
  private def tuppleToStr(input: List[(String, Int, String)]): String = {
    if (input.size > 9) {
      val res  = input.take(10).map(x =>  "(" + x._1 +", " + x._2 + ", " + x._3 + ")").mkString("\r\n").trim
      return res
    } else if (input.size > 0) {
      val res = input.map(x => "(" + x._1 + ", " + x._2 + ", " + x._3 + ")").mkString("\r\n").trim
      return res
    } else {
      return "None"
    }
  }


}
