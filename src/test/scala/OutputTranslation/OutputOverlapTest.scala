package OutputTranslation

import UtilityClasses.{OutputEntry, OutputEntryFrequency}
import UtilityClasses.OutputEntryFrequency.Junda
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.SortedMap

class OutputOverlapTest extends AnyFlatSpec with Matchers {


  it should "check junda Single" in {
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
jjjh 6185 蓍 6185, 薑 6595, 莤 7017, 葙 7361, 蓸 8633, 薔 8638, 蘁 8815, 荺 0, 荁 0, 蘛 0
yhds 6247 锾 6247, 铥 7445, 镵 8066, 钑 8767, 𬬮 0
pxjh 6310 鲭 6310, 鲳 6372, 鮊 6549, 鲴 6565, 鯝 7144, 鲊 7344, 鯡 7717, 鳣 7781, 鮨 7927, 鱣 7928
pxjo 6451 鳜 6451, 鱇 6484, 鲽 6555, 鲦 6606, 鳈 6667, 鮡 6844, 鰜 6922, 鯮 6926, 鳀 6931, 鱵 7119
yhgo 6536 铗 6536, 铼 7005, 锳 7957, 𫟹 0, 𫓹 0, 𫓧 0, 𨱑 0, 䦄 0""".replaceAll("\\s", "")

    secondTest.replaceAll("\\s", "") shouldBe """
jhxwwo 8511 駃 8511, 騠 8513, 騛 8552, 駷 8727, 騄 9186, 駣 9650, 駥 9651, 驖 9660, 驥 9661, 騍 9789
nhxwwo 8511 駃 8511, 騠 8513, 騛 8552, 駷 8727, 騄 9186, 駣 9650, 駥 9651, 驖 9660, 驥 9661, 騍 9789
ohcyjo 9307 鎍 9307, 鐷 9736, 鍱 9913, 錸 0, 鏌 0, 錤 0, 鎱 0, 鋉 0, 鏾 0, 鐼 0
hhxjgo 2147483647 䛤 0, 䛕 0, 䛊 0
jhxwwg 2147483647 駱 0, 駒 0, 駟 0, 騆 0, 駘 0, 驨 0, 駧 0, 騞 0, 馰 0, 䮰 0
jhxwwh 2147483647 驓 0, 駽 0, 駔 0, 䯀 0, 䮾 0, 䮺 0, 䮩 0, 䮢 0, 䮞 0, 䮖 0
jhxwwj 2147483647 駻 0, 騲 0, 驔 0, 馯 0, 駍 0, 騈 0, 騨 0, 馸 0, 䮺 0, 䮨 0
jhxwwm 2147483647 䣖 0
jhxwwn 2147483647 驏 0, 騜 0, 騹 0, 駈 0, 䮵 0, 䮳 0, 䮤 0, 䮠 0
jhxwwp 2147483647 駫 0, 馾 0, 䮭 0, 䮘 0, 䭺 0""".replaceAll("\\s", "")

    jundaFour.length shouldBe 467
    jundaSix.length shouldBe 48
    val test22 = ""
  }

  it should "check tzai Single" in {
    val tzaiFour = OutputOverlapObjects.singleTzaiAboveNineFourCode
    val tzaiSix = OutputOverlapObjects.singleTzaiAboveNineSixCode

    val firstTest: String = OutputOverlapObjects.singleStrFromList(tzaiFour.take(10))
    val secondTest: String = OutputOverlapObjects.singleStrFromList(tzaiSix.take(10))

    val jnxo = OutputSorting.mapFullTzai.get("jnxo")
    val kxho = OutputSorting.mapFullTzai.get("kxho")
    val jjjh = OutputSorting.mapFullTzai.get("jjjh")
    val xjgo = OutputSorting.mapFullTzai.get("xjgo")
    val xhjo = OutputSorting.mapFullTzai.get("xhjo")

    val jjxo = OutputSorting.mapFullTzai.get("jjxo")

    val jhxwwo = OutputSorting.mapFullTzai.get("jhxwwo")
    val nhxwwo = OutputSorting.mapFullTzai.get("nhxwwo")
    val ohcyxo = OutputSorting.mapFullTzai.get("ohcyxo")
    val ohcyjo = OutputSorting.mapFullTzai.get("ohcyjo")
    val ohcyko = OutputSorting.mapFullTzai.get("ohcyko")

    val res1 = OutputOverlapObjects.outputElevenEntryNestedListToString(
      List("jnxo", "kxho", "jjjh", "xjgo", "xhjo"),
      List(jnxo, kxho, jjjh, xjgo, xhjo), OutputEntryFrequency.Tzai)

    res1.replaceAll("\\s", "") shouldBe
      """
       jnxo 歡 246, 散 1009, 鞭 2440, 艱 2587, 歎 2649, 蔑 3192, 鞠 3354, 鞦 4394, 顢 5123, 鞅 5412
        kxho 真 95, 碟 531, 硬 593, 碩 1100, 頁 1309, 礙 1717, 礎 1782, 顛 2354, 磺 4346, 饜 5477
        jjjh 著 177, 址 1137, 扯 1692, 薔 3305, 躇 3625, 薑 3631, 葫 3772, 茸 4331, 苣 5615, 荺 5624
        xjgo 戰 331, 員 345, 興 357, 嚴 805, 喂 1946, 噪 2531, 唄 4185, 顎 4742, 噥 5278, 嘳 5724
        xhjo 是 2, 果 99, 題 105, 味 709, 暴 871, 顆 1051, 煚 3373, 嘖 3684, 嗷 4578, 暪 5811""".replaceAll("\\s", "")

    firstTest.replaceAll("\\s", "") shouldBe
      """
jnxo 5412 鞅 5412, 顴 5454, 茦 5879, 蔌 6853, 蓛 8187, 蕀 8641, 鞣 8818, 韇 9286, 靺 9664, 蔈 10379
kxho 5477 饜 5477, 戛 5769, 礞 6560, 硤 7317, 矙 7439, 磌 8205, 礣 8305, 磧 8356, 礥 8733, 睼 9636
jjjh 5624 荺 5624, 莤 6434, 蓍 6845, 蘁 7032, 葙 7333, 荁 8917, 蘛 9244, 芏 9339, 躤 12759, 苷 0
xjgo 5724 嘳 5724, 囅 8325, 辴 8725, 哫 8887, 爨 9314, 戙 10087, 嚗 10482, 喿 10901, 爂 12337, 颚 0
xhjo 5811 暪 5811, 杲 6045, 嗉 6229, 昺 6787, 曚 6866, 暵 7865, 晸 8142, 嚽 9257, 暕 10909, 褁 12078
wjgo 5821 漯 5821, 灦 6175, 淏 6807, 湜 6969, 澖 6992, 浿 7744, 淟 8463, 渨 8974, 潩 9689, 溳 10262
wjgh 6234 澠 6234, 淐 7300, 灗 7670, 湒 8124, 駏 9105, 泪 0, 濐 0, 㶄 0, 㵎 0, 㴘 0
pxjo 6470 鯕 6470, 鰷 6649, 鱌 6651, 鱳 6887, 鯀 7234, 鰬 7425, 鮢 7917, 鮇 8654, 鮡 8682, 魰 9111
djgo 6856 嬠 6856, 梀 9445, 娖 10745, 姎 11347, 嬇 11619, 婰 11995, 棫 12050, 孍 12752, 娱 0, 娲 0
jjxo 6869 藈 6869, 虆 7042, 菋 8514, 蔂 8612, 躉 8819, 葨 9016, 蹎 9816, 薎 11137, 蔝 11638, 蕺 12296""".replaceAll("\\s", "")

    secondTest.replaceAll("\\s", "") shouldBe
      """
jhxwwo 5168 騾 5168, 驃 5281, 騄 5405, 騬 6128, 騋 6637, 騃 7017, 騵 7696, 騥 7948, 騠 7949, 騤 8298
nhxwwo 5168 騾 5168, 驃 5281, 騄 5405, 騬 6128, 騋 6637, 騃 7017, 騵 7696, 騥 7948, 騠 7949, 騤 8298
ohcyxo 9186 鍡 9186, 鐰 9262, 錪 10444, 鑤 11313, 鉠 11561, 鉂 0, 鉙 0, 淾 0, 鐛 0, 鋜 0
ohcyjo 9214 鎍 9214, 鎱 9215, 鐷 9263, 鋉 9723, 鏾 11238, 鐼 11257, 鑶 11299, 鑟 0, 鏉 0, 鐄 0
ohcyko 9662 鋮 9662, 銊 12924, 鐝 0, 龯 0, 鏯 0
jhxwwj 10557 騲 10557, 驔 10591, 馯 11563, 駍 11653, 驌 12751, 騈 0, 騨 0, 馸 0, 䮺 0, 䮨 0
nhxwwj 10557 騲 10557, 驔 10591, 馯 11563, 駍 11653, 驌 12751, 騈 0, 騨 0, 馸 0, 䮺 0, 䮨 0
ohcyjn 10998 銈 10998, 鋍 11648, 鑉 12749, 鍢 0, 𨧀 0, 鐂 0, 䥓 0
jhxwws 11244 騩 11244, 馰 12902, 駳 0, 驋 0, 駊 0, 駀 0, 馶 0, 䯁 0, 䮱 0, 䮮 0
nhxwws 11244 騩 11244, 馰 12902, 駳 0, 驋 0, 駊 0, 駀 0, 馶 0, 䯁 0, 䮱 0, 䮮 0""".replaceAll("\\s", "")

    tzaiFour.length shouldBe 467
    tzaiSix.length shouldBe 48
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

    val xhx = OutputSorting.mapFullJunda.get("xhx")
    val jow = OutputSorting.mapFullJunda.get("jow")
    val kow = OutputSorting.mapFullJunda.get("kow")
    val uou = OutputSorting.mapFullJunda.get("uou")
    val tow = OutputSorting.mapFullJunda.get("tow")

    val jjjjj = OutputSorting.mapFullJunda.get("jjjjj")
    val jjjjg = OutputSorting.mapFullJunda.get("jjjjg")
    val dofhz = OutputSorting.mapFullJunda.get("dofhz")
    val wertn = OutputSorting.mapFullJunda.get("wertn")
    val hhjjo = OutputSorting.mapFullJunda.get("hhjjo")

    val res1 = OutputOverlapObjects.outputElevenEntryNestedListToString(
      List("xhx", "jow", "kow", "uou", "tow"),
      List(xhx, jow, kow, uou, tow), OutputEntryFrequency.BCLU)

    val res2 = OutputOverlapObjects.outputElevenEntryNestedListToString(
      List("jjjjj", "jjjjg", "dofhz", "wertn", "hhjjo"),
      List(jjjjj, jjjjg, dofhz, wertn, hhjjo), OutputEntryFrequency.BCLU)

    res1.replaceAll("\\s", "") shouldBe
      """
xhx 明显 484, 昨日 696, 明星 1711, 昨晚 1955, 明明 3466, 口味 4610, 回收 5110, 回国 5540, 口号 5579, 呜呜 5637
jow 其实 337, 政治 708, 教学 1074, 真实 1550, 联赛 1634, 其它 1721, 荣誉 4518, 震惊 4963, 教室 5233, 黄河 5702
kow 感觉 356, 欧洲 1283, 感情 1328, 真实 1550, 感染 1949, 顾客 2966, 眼泪 2973, 原油 3493, 感慨 5567, 感激 5985
uou 价 545, 休息 1686, 集体 1707, 货币 2417, 伙伴 3302, 似的 3547, 倾向 4065, 保健 4273, 促使 4676, 顺便 6106
tow 决定 394, 资源 592, 旅游 845, 交流 1201, 决赛 2211, 文学 2474, 文字 2744, 旅客 3135, 病情 4238, 亲密 6260""".replaceAll("\\s", "")

    firstTest.replaceAll("\\s", "") shouldBe """
xhx 5637 呜呜 5637, 贿赂 6639, 围巾 7097, 明日 7875, 口岸 8186, 口水 8232, 骨髓 9193, 暗中 9391, 回回 9661, 胃口 9696
jow 5702 黄河 5702, 救灾 7912, 救治 8230, 真情 8787, 政法 8843, 救济 9792, 真空 10145, 裁定 10907, 故宫 12603, 索性 12610
kow 5985 感激 5985, 感性 8576, 真情 8787, 原定 9512, 真空 10145, 感悟 10666, 爽快 15140, 碱性 16131, 感官 17541, 眼帘 20751
uou 6106 顺便 6106, 保佑 7330, 保住 10366, 供货 10676, 休假 10924, 保修 15699, 倾倒 17729, 做作 17788, 供血 22744, 依偎 24681
tow 6260 亲密 6260, 资深 7199, 亲情 7666, 放学 9925, 放宽 10390, 交涉 12855, 交割 13525, 放慢 14026, 装满 15097, 廉洁 15398
kox 6381 感叹 6381, 眼圈 10738, 成败 12712, 硬是 13465, 眼见 13939, 眼影 15592, 眼界 17864, 成因 19176, 残暴 23375, 残骸 23446
jhj 6658 本事 6658, 古老 6966, 坦克 7082, 古城 9486, 雪花 9702, 正要 11130, 葫芦 12319, 本本 12410, 甘草 13049, 坍塌 14536
jox 6663 茶叶 6663, 跳水 8434, 职员 9432, 歌唱 9647, 跳出 9836, 联网 9896, 雨水 10106, 索赔 10492, 共鸣 11995, 踪影 12775
joj 7103 蒙古 7103, 荣获 7632, 菊花 7688, 歌声 7744, 栽培 8108, 职场 8686, 芙蓉 9614, 款项 9994, 城墙 10398, 颠覆 10507
wot 7225 额度 7225, 演变 7829, 溃疡 9180, 实效 12267, 实况 17678, 泳装 19138, 淡忘 19600, 家畜 21953, 家产 22076, 家底 25418""".replaceAll("\\s", "")

    res2.replaceAll("\\s", "") shouldBe
      """
jjjjj 劳斯莱斯 30400, 塔吉克斯坦 34769, 雷克萨斯 50495, 斯芬克斯 198522, 萬事起頭難 0, 華茲華斯 0, 雷克薩斯 0, 埋頭苦幹 0, 聖地亞哥 0, 草草 16241
jjjjg 走下坡路 36798, 刺芹菇 0, 薄荷酮 0, 才華蓋世 0, 聖荷西 0, 跌跌蹌蹌 0, 歡歡喜喜 0, 亞達薛西 0, 薩蘭斯克 0, 赶超 21998
dofhz 妹子 5780, 猴子 8730, 架子 12887, 娘子 14322, 桃子 20133, 棋子 21031, 林子 22657, 婊子 23016, 模子 35061, 榛子 41056
wertn 温州市 19617, 漳州市 30288, 湖州市 45131, 宿州市 49546, 沧州市 52347, 泸州市 62025, 忻州市 63651, 潮州市 64446, 滁州市 65237, 滨州市 72309
hhjjo 一神教 0, 禍福與共 0, 三三兩兩 0, 三天兩頭 0, 青藏 10735, 青菜 13849, 甜菜 33118, 三藏 45860, 春茶 50231, 青蒜 81400""".replaceAll("\\s", "")

    secondTest.replaceAll("\\s", "") shouldBe """
jjjjj 16241 草草 16241, 草莽 60224, 莽草 116443, 莘莘 0, 葎草 0, 南華 0, 薺苧 0, 薺薴 0
jjjjg 21998 赶超 21998, 草菇 55018, 幸喜 101140, 革吉 325548, 藓苔 0, 趕超 0, 幹警 0, 蘚苔 0
dofhz 41056 榛子 41056, 核子 54143, 禁卫 59669, 楔子 77100, 椽子 96363, 戳子 197696, 橡子 0, 槟子 0, 樣子 0, 機子 0
wertn 72309 滨州市 72309, 池州市 77437, 汝州市 82592, 定州市 93725, 涿州市 102823, 深州市 159548, 清州市 565366, 溫州市 0, 滄州市 0, 濱州市 0
hhjjo 81400 青蒜 81400, 三芝 0, 甜菊 0, 春藥 0, 三蘇 0
wowkt 85472 兴海 85472, 案头 0, 渠沟 0, 濒海 0, 溟海 0, 憷头 0, 淺海 0, 濱海 0, 瀕海 0
nonjo 101366 虔敬 101366, 敬茶 0, 藤菜 0, 蒙藥 0, 茶葉 0, 艾葉 0, 蘇菜 0, 紫蘇 0, 蕺菜 0, 鹹菜 0
joxht 102344 荣县 102344, 藤县 106459, 共时 0, 某时 0, 索国 0
xhwkt 105874 明沟 105874, 唱头 281911, 日没 0, 冒头 0, 日沒 0
jpjjm 106945 芫花 106945, 花萼 107019, 老花 0""".replaceAll("\\s", "")

    BCLUthree.length shouldBe 3836
    BCLUfive.length shouldBe 334
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
    val jot = OutputSorting.mapFullTzai.get("jot")
    val jsu = OutputSorting.mapFullTzai.get("jsu")
    val toj = OutputSorting.mapFullTzai.get("toj")

    val dhfhz = OutputSorting.mapFullTzai.get("dhfhz")
    val dnfhz = OutputSorting.mapFullTzai.get("dnfhz")
    val dodzz = OutputSorting.mapFullTzai.get("dodzz")
    val dofhz = OutputSorting.mapFullTzai.get("dofhz")
    val dojho = OutputSorting.mapFullTzai.get("dojho")

    val res1 = OutputOverlapObjects.outputElevenEntryNestedListToString(
      List("uox", "joj", "jot", "jsu", "toj"),
      List(uox, joj, jot, jsu, toj), OutputEntryFrequency.Sinica)

    val res2 = OutputOverlapObjects.outputElevenEntryNestedListToString(
      List("dhfhz", "dnfhz", "dodzz", "dofhz", "dojho"),
      List(dhfhz, dnfhz, dodzz, dofhz, dojho), OutputEntryFrequency.Sinica)

    res1.replaceAll("\\s", "") shouldBe
      """
uox 集團 1431, 集中 1444, 樂團 1981, 樂器 2198, 依照 2317, 傑出 2491, 做出 3006, 便是 3144, 樂園 3284, 儀器 3308
joj 東西 310, 或者 519, 故事 567, 真正 687, 蘇聯 885, 頭髮 2517, 歌聲 3569, 執著 3618, 茶葉 3864, 東亞 4078
jot 政府 122, 教育 180, 英文 1488, 其次 1908, 東方 2101, 東京 2256, 故意 2526, 東部 3522, 頭痛 4093, 長度 4374
jsu 報紙 1502, 連線 1607, 取代 1991, 連續 2091, 取締 2528, 蒐集 2548, 支付 3269, 邁向 3552, 取向 3819, 連結 4591
toj 之下 822, 廣場 1297, 變遷 2496, 資本 2654, 豪華 3813, 放下 3901, 變更 4324, 凝聚 4534, 文教 4579, 痕跡 4608""".replaceAll("\\s", "")

    firstTest.replaceAll("\\s", "") shouldBe """
uox 3308 儀器 3308, 樂曲 5267, 集體 5445, 供水 6827, 使出 7378, 貨品 7437, 係數 7868, 樂山 0, 便中 0, 保山 0
joj 4078 東亞 4078, 長達 4218, 執政 4381, 歡喜 4567, 緊緊 4865, 與其 5002, 東歐 5139, 長遠 5758, 越南 5841, 栽培 6508
jot 4374 長度 4374, 共享 5176, 頭部 6959, 長庚 7254, 共產 8048, 散文 8651, 兩旁 8819, 執意 8864, 真意 0, 長於 0
jsu 4591 連結 4591, 連絡 5795, 零件 5868, 邁進 5944, 零售 6562, 蔓延 7536, 報信 0, 取信 0, 取經 0, 取樂 0
toj 4608 痕跡 4608, 變革 5213, 麻醉 5607, 文藝 5637, 廣東 5735, 放鬆 5926, 旗下 6265, 次長 7115, 京都 8314, 文革 8947
uou 4897 順便 4897, 綠島 5260, 供給 5424, 條約 5609, 候鳥 5689, 保健 6057, 休假 6689, 傢伙 6712, 綠化 6958, 傾倒 6989
jox 4913 蘋果 4913, 藥品 5555, 雨水 6049, 歌唱 6851, 跳水 7259, 共鳴 7298, 賣出 8085, 歌星 9511, 真是 0, 真題 0
xhj 5415 日趨 5415, 骨頭 6781, 回教 8049, 回報 8950, 困惑 9446, 日電 0, 明教 0, 回電 0, 回本 0, 回事 0
jow 5544 緊密 5544, 長官 5642, 東海 5808, 欺騙 6072, 震盪 6078, 教官 6437, 填寫 7541, 政客 7895, 執法 8118, 救濟 8551
tox 5646 放置 5646, 放出 7786, 放映 7788, 變異 8939, 旗幟 8944, 放過 9333, 資中 0, 交出 0, 交還 0, 交點 0""".replaceAll("\\s", "")

    res2.replaceAll("\\s", "") shouldBe
      """
dhfhz 女子 1665, 李子 0, 麵子 0, 猶子 0, 棚子 0, 柱子 0, 柵子 0, 杏子 0, 柚子 0, 柜子 0
dnfhz 檔子 0, 娃子 0, 狸子 0, 柵子 0, 杆子 0, 檯子 0, 姪子 0, 柿子 0, 栓子 0, 嬸子 0
dodzz 林木 0, 飛刀 0, 橫木 0, 槓刀 0, 橡木 0, 戮力 0, 槭木 0, 棶木 0, 横木 0, 枫木 0
dofhz 樣子 1194, 猴子 6474, 核子 7248, 機子 0, 林子 0, 妹子 0, 模子 0, 架子 0, 娘子 0, 棋子 0
dojho 校長 744, 機長 0, 來項 0, 負載 0, 桃城 0, 翼城 0, 狹長 0, 檳城 0, 根鬚 0, 槟城 0""".replaceAll("\\s", "")
    
    secondTest.replaceAll("\\s", "") shouldBe """
dhfhz 2147483647 柜子 0, 杓子 0, 杠子 0, 楦子 0, 猸子 0, 档子 0, 栅子 0, 女卫 0
dnfhz 2147483647 嬸子 0
dodzz 2147483647 枫木 0, 梾木 0
dofhz 2147483647 棋子 0, 桃子 0, 檳子 0, 槓子 0, 戳子 0, 橡子 0, 婊子 0, 楔子 0, 榛子 0, 椽子 0
dojho 2147483647 槟城 0, 杂项 0
dojjo 2147483647 林茨 0, 横越 0
dowgo 2147483647 禁赛 0, 棋赛 0
doxho 2147483647 賀縣 0, 棋具 0, 頗具 0, 穎果 0, 榛果 0, 横是 0, 颖果 0, 颇具 0, 横暴 0
dyhgj 2147483647 加利福尼亚大学洛杉矶分校 0
dyqgj 2147483647 加利福尼亚大学洛杉矶分校 0""".replaceAll("\\s", "")
    
    SINICAthree.length shouldBe 3836
    SINICAfive.length shouldBe 334
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

    val qnh = OutputSorting.mapFullTzai.get("qnh")
    val cht = OutputSorting.mapFullTzai.get("cht")
    val fnh = OutputSorting.mapFullTzai.get("fnh")
    val mdw = OutputSorting.mapFullTzai.get("mdw")
    val dty = OutputSorting.mapFullTzai.get("dty")

    val towho = OutputSorting.mapFullTzai.get("towho")
    val lnkxh = OutputSorting.mapFullTzai.get("lnkxh")
    val wojho = OutputSorting.mapFullTzai.get("wojho")
    val towgo = OutputSorting.mapFullTzai.get("towgo")
    val jtjno = OutputSorting.mapFullTzai.get("jtjno")

    val res1 = OutputOverlapObjects.outputElevenEntryNestedListToString(
      List("qnh", "cht", "fnh", "mdw", "dty"),
      List(qnh, cht, fnh, mdw, dty), OutputEntryFrequency.BCLU)

    val res2 = OutputOverlapObjects.outputElevenEntryNestedListToString(
      List("towho", "lnkxh", "wojho", "towgo", "jtjno"),
      List(towho, lnkxh, wojho, towgo, jtjno), OutputEntryFrequency.BCLU)
    
    res1.replaceAll("\\s", "") shouldBe
      """
qnh 福祉 30426, 福瑞 0, 袖珍 30592, 裡脊 0, 祉祿 0, 评理 71462, 训示 85188, 福寿 650981, 训戒 0, 让 69
cht 情意 21231, 情資 0, 情變 0, 情商 0, 情敵 0, 情癡 0, 情痴 78927, 怕癢 0, 怕痒 108120, 情况 159
fnh 出現 0, 籃球 0, 出資 0, 出神 30015, 阿三 0, 出示 9373, 出于 4577, 阿瑟 66894, 墜琴 0, 出现 191
mdw 比賽 0, 比容 98754, 比安 555614, 切實 0, 切激 0, 房室 28791, 房客 25389, 切割 12729, 比濕 0, 比赛 220
dty 力度 1932, 皮重 147798, 皮毛 21262, 刀庫 0, 皮秒 0, 娟秀 77250, 妖氣 0, 妖物 70129, 妖气 53736, 发生 240""".replaceAll("\\s", "")

    firstTest.replaceAll("\\s", "") shouldBe """
qnh 69 让 69
cht 159 情况 159, 情状 56117, 情变 0, 情资 0, 当夜 35022, 阎魔 0, 间充 0
fnh 191 出现 191, 出动 7098, 出击 7817, 篮球 4157, 凹进 62459, 陆丰 47014, 陆运 77625
mdw 220 比赛 220, 比湿 173709, 轮空 36561, 轮流 11135, 切实 2826, 轮滑 0, 轮渡 23726
dty 240 发生 240, 飞手 0, 飞舞 16369, 发毛 44396, 刀库 0, 飞升 29875, 村镇 16483, 村长 16607, 栈租 0, 发愁 15806
qyi 270 必须 270
ytx 311 我国 311, 失败 2049
oog 358 领导 358, 从属 30934, 关张 62537
htl 633 未来 633, 未报 0, 违抗 43656, 麦霸 0, 迂执 501344, 违拗 102166, 进来 3264, 远扬 52729, 运势 350284
ctp 680 快乐 680, 快鱼 0, 闲逸 102602, 闲逛 25672, 闲杂 59455""".replaceAll("\\s", "")

    res2.replaceAll("\\s", "") shouldBe
      """
towho 資源 0, 病源 50208, 廣漢 0, 族滅 0, 資淺 0, 廉潔 0, 廣漠 0, 衰減 0, 褻瀆 0, 资源 592
lnkxh 擁有 0, 平面 7504, 平直 42212, 握有 0, 扯直 0, 拙直 624576, 挂面 50939, 拌面 0, 抻面 152177, 拥有 649
wojho 家長 0, 深長 0, 源城 0, 漢城 0, 滿載 0, 涼城 0, 濱城 0, 兴城 67787, 滨城 68943, 实践 1362
towgo 六家 0, 病家 79562, 文宗 84049, 廣宗 0, 廠家 0, 哀家 0, 庚寅 63227, 亲家 31496, 决定 394, 决赛 2211
jtjno 豆瓣菜 270516, 求職 0, 求歡 0, 去職 0, 走散 46984, 艾葉 0, 去职 57969, 求职 10557, 苏共 0, 苏联 2455""".replaceAll("\\s", "")
    
    secondTest.replaceAll("\\s", "") shouldBe """
towho 592 资源 592, 装潢 16524, 装满 15097, 亵渎 26872, 资浅 0
lnkxh 649 拥有 649
wojho 1362 实践 1362, 满载 15865
towgo 2211 决赛 2211
jtjno 2455 苏联 2455
poxho 3277 风景 3277
xotpo 7727 贵族 7727, 内衣 5600, 贵庚 126422
jsozz 11917 过人 11917
jtjjg 12319 葫芦 12319
jsnjo 12608 菠菜 12608, 芍藥 0""".replaceAll("\\s", "")

    BCLUthree.length shouldBe 3836
    BCLUfive.length shouldBe 334
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

    val lsi = OutputSorting.mapFullJunda.get("lsi")
    val ugi = OutputSorting.mapFullJunda.get("ugi")
    val soi = OutputSorting.mapFullJunda.get("soi")
    val twk = OutputSorting.mapFullJunda.get("twk")
    val lmx = OutputSorting.mapFullJunda.get("lmx")

    val lnkxh = OutputSorting.mapFullJunda.get("lnkxh")
    val joxht = OutputSorting.mapFullJunda.get("joxht")
    val ynfhz = OutputSorting.mapFullJunda.get("ynfhz")
    val woozz = OutputSorting.mapFullJunda.get("woozz")
    val lhxho = OutputSorting.mapFullJunda.get("lhxho")

    val res1 = OutputOverlapObjects.outputElevenEntryNestedListToString(
      List("lsi", "ugi", "soi", "twk", "lmx"),
      List(lsi, ugi, soi, twk, lmx), OutputEntryFrequency.Sinica)
    
    val res2 = OutputOverlapObjects.outputElevenEntryNestedListToString(
      List("lnkxh", "joxht", "ynfhz", "woozz", "lhxho"),
      List(lnkxh, joxht, ynfhz, woozz, lhxho), OutputEntryFrequency.Sinica)
    
    res1.replaceAll("\\s", "") shouldBe
      """
lsi 报德 0, 授衔 0, 投契 0, 授乳 0, 报盘 0, 摆盘 0, 霞径 0, 授證 0, 搜證 0, 技術 241
ugi 向往 0, 向后 0, 信徒 5237, 他律 0, 向盘 0, 向後 0, 結彩 0, 結論 2076, 統御 0, 的話 356
soi 以后 0, 以往 0, 预后 0, 预言 0, 欢欣 0, 戏言 0, 蜗行 0, 以律 0, 买房 0, 以後 415
twk 意愿 0, 熟睡 0, 夜袭 0, 夜大 0, 意态 0, 恣睢 0, 意面 0, 熟成 0, 疼死 0, 意見 436
lmx 批量 0, 批号 0, 拷贝 0, 掩映 0, 雹暴 0, 拒收 0, 掩體 0, 批點 0, 電量 0, 電影 543""".replaceAll("\\s", "")

    firstTest.replaceAll("\\s", "") shouldBe """
lsi 241 技術 241, 搬請 0, 授計 0, 雲彩 0, 雲盤 0, 授課 4811, 投遞 0, 投訴 0, 投誠 0, 授銜 0
ugi 356 的話 356, 俗話 0, 統計 1414, 俗語 0, 結語 0, 統讀 0, 結訓 0, 結識 0, 結記 0, 紀律 8881
soi 415 以後 415, 柔術 0, 預言 0, 預後 0, 預計 1490, 預設 0, 預試 0, 戳記 0, 預訂 8336, 蟻后 0
twk 436 意見 436, 鷹犬 0, 意願 1815, 應有 0, 憑眺 0
lmx 543 電影 543, 電器 9353, 電唱 0, 電唁 0, 批號 0, 擺出 0, 匯出 0, 擺明 0, 匯回 0, 匯水 0
mmw 633 比賽 633, 比濕 0
dfd 814 婦女 814, 榔槺 0, 婦檢 0, 嫏嬛 0
tso 880 適合 880, 遊人 0, 贏餘 0, 廢人 0, 就學 8626, 凌亂 0, 遊學 0
upf 902 兒子 902, 化妝 6447, 兒孫 0
tjy 944 新竹 944, 新和 0, 净重 0, 冲销 0, 冲程 0, 新丰 0, 冲积 0, 亭长 0, 净手 0, 辩辞 0""".replaceAll("\\s", "")

    res2.replaceAll("\\s", "") shouldBe
      """
lnkxh 拥有 0, 平面 3739, 平直 0, 挂面 0, 抻面 0, 拙直 0, 握有 0, 扯直 0, 拌面 0, 擁有 560
joxht 英国 0, 救国 0, 卖国 0, 赵国 0, 越国 0, 故国 0, 赵县 0, 萧县 0, 茂县 0, 荣县 0
ynfhz 钉子 0, 罐子 0, 锥子 0, 壬子 0, 重子 0, 稚子 0, 镏子 0, 季子 0, 程子 0, 種子 1942
woozz 深入 1511, 家人 1336, 惊人 0, 懒人 0, 浪人 0, 满人 0, 淡入 0, 恨人 0, 慎入 0, 漢人 2247
lhxho 拉夫罗夫 0, 拉夫羅夫 0, 霜晨 0, 雪暴 0, 雪景 0, 扣题 0, 扣題 0, 抬顯 0, 霸縣 0, 指數 2327""".replaceAll("\\s", "")


    secondTest.replaceAll("\\s", "") shouldBe """
lnkxh 560 擁有 560
joxjy 908 英國 908, 越國 0, 故國 0, 索國 0, 救國 0, 貢國 0, 賣國 0, 兩國 0
ynfhz 1942 種子 1942
woozz 2247 漢人 2247, 滿人 0, 懶人 0, 駭人 0
lhxho 2327 指數 2327, 招數 0
jhjnh 2397 藉著 2397, 古舊 0, 聶耳 0
jhnjh 2397 藉著 2397, 古舊 0
toxho 4002 次數 4002, 哀嘆 0, 變數 4703
jnfhz 4505 莊子 4505, 墊子 0
jgjjn 5836 菩薩 5836""".replaceAll("\\s", "")

    SINICAthree.length shouldBe 3836
    SINICAfive.length shouldBe 334
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
