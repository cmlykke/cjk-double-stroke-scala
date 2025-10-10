package Atypes

import UtilityClasses.Cluster


object Aelements {

  val elementTypes: Set[Aelementstype] = Set(
    new Aelementstype(Set("1234"), Set("木")),
    new Aelementstype(Set("1234"), Set("⽊")),
    new Aelementstype(Set("251(215|2121)", "2512134"), Set("⿱口止")),
    new Aelementstype(Set("251(215|2121)", "2512134"), Set("足")),
    new Aelementstype(Set("2512121", "2512134"), Set("⻊")),
    new Aelementstype(Set("251(215|2121)", "2512134"), Set("⾜")),

    new Aelementstype(Set("314314"), Set("⿰⿱𠂊亅⿱𠂊亅")),
    new Aelementstype(Set("314314"), Set("竹")),
    new Aelementstype(Set("314314"), Set("⺮")),
    new Aelementstype(Set("251214"), Set("虫")),
    new Aelementstype(Set("251214"), Set("⾍")),

    new Aelementstype(Set("3112"), Set("手")),
    new Aelementstype(Set("3112"), Set("⼿")),
    new Aelementstype(Set("121"), Set("扌")),
    new Aelementstype(Set("121"), Set("⺘")),

    new Aelementstype(Set("25111"), Set("目")),

    new Aelementstype(Set("(1|4)111251"), Set("言")),
    new Aelementstype(Set("(1|4)111251"), Set("訁")),
    new Aelementstype(Set("(1|4)111251"), Set("⾔")),

    new Aelementstype(Set("(554234|554444)"), Set("⿱⿰②丶③")),
    new Aelementstype(Set("(554234|554444)"), Set("糸")),
    new Aelementstype(Set("(554234|554444)"), Set("糹")),
    new Aelementstype(Set("(554234|554444)"), Set("⺯")),
    new Aelementstype(Set("(554234|554444)"), Set("⽷")),

    new Aelementstype(Set("34112431"), Set("⿱人⿻⿱一⿱十一丷")),
    new Aelementstype(Set("34112431"), Set("金")),
    new Aelementstype(Set("34112431"), Set("⾦")),

    new Aelementstype(Set("25112511"), Set("⿰𠁣𠃛")),
    new Aelementstype(Set("25112511"), Set("門")),
    new Aelementstype(Set("25112511"), Set("⾨")),

    new Aelementstype(Set("(12|21)11254444"), Set("⿹⑥灬")),
    new Aelementstype(Set("(12|21)11254444"), Set("馬")),
    new Aelementstype(Set("(12|21)11254444"), Set("⾺")),

    new Aelementstype(Set("34(1|4)(51154|511211)"), Set("⿱人⿱丶⑤")),
    new Aelementstype(Set("34(1|4)(51154|511211)"), Set("食")),
    new Aelementstype(Set("34(1|4)(51154|511211)"), Set("飠")),
    new Aelementstype(Set("34(1|4)(51154|511211)"), Set("⾷")),
    new Aelementstype(Set("34(1|4)(51154|511211)"), Set("⻝")),
    new Aelementstype(Set("34(1|4)(51154|511211)"), Set("⻞")),
    new Aelementstype(Set("34(1|4)(51154|511211)"), Set("⻟")),

    new Aelementstype(Set("1251112"), Set("車")),
    new Aelementstype(Set("1251112"), Set("⾞")))

  val idsToStrokeMap: Map[String, Set[String]] = {
    elementTypes
      .flatMap(elem => elem.ids.map(id => (id, elem))) // Pair each ID with its Aelementstype
      .groupMap(_._1)(_._2) // Group by ID, collecting Aelementstype objects
      .view
      .mapValues(_.flatMap(_.strokes).toSet) // Extract and flatten strokes into a Set
      .toMap // Materialize as immutable Map
  }
}


