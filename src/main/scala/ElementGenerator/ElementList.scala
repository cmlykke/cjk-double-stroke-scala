package ElementGenerator

import UtilityClasses.Cluster

class ElementList {

}

object ElementList {
  val elementTypes: List[ElementType] = List(
    new ElementType("1234", new Cluster("木"), "a"),
    //overlap point: Junda 2919梗  Tzai
    new ElementType("251(215|2121)", new Cluster("⿱口止"), "b"),
    //overlap point: Junda 2977趴  Tzai
    new ElementType("314314", new Cluster("⿰⿱𠂊亅⿱𠂊亅"), "c"), //'⺮' 11950
    //overlap point: Junda 3785筏  Tzai
    new ElementType("251214", new Cluster("虫"), "d"),
    //overlap point: Junda 3918蜈  Tzai
    new ElementType("121", new Cluster("扌"), "e"),
    //overlap point: Junda 4299摞  Tzai
    new ElementType("25111", new Cluster("目"), "f"),
    //overlap point: Junda 4742嗪 -- 5846瞋  Tzai --目  25111(12251111|13251111|15251115|35251115|53251115)34

    //new ElementType("251", new Cluster("口"), "g"),
    //嗉 5351 -- 題 5105
    //new ElementType("122111", new Cluster("耳"), "h"),
    //聩 5209 -- 122111251212534  耳
    //next highest overlap: 糇 junda 5898

    new ElementType("(1|4)111251", new Cluster("言"), "k"),
    //overlap point: Junda   tzai 誠  894
    new ElementType("(554234|554444)", new Cluster("⿱⿰②丶③"), "l"),
    //overlap point: Junda   tzai  縱 1448  (554234|554444)33234342134   ⿱⿰②丶③
    new ElementType("34112431", new Cluster("⿱人⿻⿱一⿱十一丷"), "m"),
    //overlap point: Junda   tzai 錶 2473  // ⿱人⿻⿱一⿱十一丷   3411243111213534
    new ElementType("25112511", new Cluster("⿰𠁣𠃛"), "n"),
    //overlap point: Junda   tzai tzai 曙 3429 -- 閂 5040 //⿰𠁣𠃛  251125111
    new ElementType("(12|21)11254444", new Cluster("⿹⑥灬"), "o"),
    //overlap point: Junda   tzai  驤 4204 (12|21)1125444441251251112213534  ⿹⑥灬
    new ElementType("34(1|4)(51154|511211)", new Cluster("⿱人⿱丶⑤"), "p"),
    //overlap point: Junda   tzai 餞 4285  34(1|4)(51154|511211)(1534|1543)\3
    new ElementType("1251112", new Cluster("車"), "q"),
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