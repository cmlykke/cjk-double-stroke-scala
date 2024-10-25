package ElementGenerator

import UtilityClasses.Cluster

class ElementList {

}

object ElementList {
  val elementTypes: Set[ElementType] = Set(
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
    //overlap point: Junda   tzai  1251112534  軔  4581  車
    //new ElementType("1221", new Cluster("革"), "r"),
    //new ElementType("1221", new Cluster("廿"), "r"),
    //overlap point: Junda   tzai  鞅 革 5412 -- 122125112321251134 -- 廿 艱 12212511134511534
    //new ElementType("13251", new Cluster("⿸⿱一丿口"), "s")
    //overlap point: Junda   tzai  5477 饜  -- 磺  ⿸⿱一丿口 132511251134

    //highes tzai overlap: 漯 5821


    // Add more elements as needed
  )

  // 251214122151234
  //  虫

  //  3143143134
  //⿰⿱𠂊亅⿱𠂊亅
}