package Adatasources.ManualData

import Atypes.Aelementstype

object Aelements {

  val elementTypes: Set[Aelementstype] = Set(
    new Aelementstype(Set("1234"), "木", "木"),
    new Aelementstype(Set("1234"), "⽊", "木"),

    new Aelementstype(Set("251(215|2121)", "2512134"), "⿱口止", "足"),
    new Aelementstype(Set("251(215|2121)", "2512134"), "足", "足"),
    new Aelementstype(Set("2512121", "2512134"), "⻊", "足"),
    new Aelementstype(Set("251(215|2121)", "2512134"), "⾜", "足"),

    new Aelementstype(Set("314314"), "⿰⿱𠂊亅⿱𠂊亅", "竹"),
    new Aelementstype(Set("314314"), "竹", "竹"),
    new Aelementstype(Set("314314"), "⺮", "竹"),

    new Aelementstype(Set("251214"), "虫", "虫"),
    new Aelementstype(Set("251214"), "⾍", "虫"),

    new Aelementstype(Set("3112"), "手", "手"),
    new Aelementstype(Set("3112"), "⼿", "手"),
    new Aelementstype(Set("121"), "扌", "手"),
    new Aelementstype(Set("121"), "⺘", "手"),

    new Aelementstype(Set("25111"), "目", "目"),

    new Aelementstype(Set("(1|4)111251"), "言", "言"),
    new Aelementstype(Set("(1|4)111251"), "訁", "言"),
    new Aelementstype(Set("(1|4)111251"), "⾔", "言"),

    new Aelementstype(Set("(554234|554444)"), "⿱⿰②丶③", "糸"),
    new Aelementstype(Set("(554234|554444)"), "糸", "糸"),
    new Aelementstype(Set("(554234|554444)"), "糹", "糸"),
    new Aelementstype(Set("(554234|554444)"), "⺯", "糸"),
    new Aelementstype(Set("(554234|554444)"), "⽷", "糸"),

    new Aelementstype(Set("34112431"), "⿱人⿻⿱一⿱十一丷", "金"),
    new Aelementstype(Set("34112431"), "金", "金"),
    new Aelementstype(Set("34112431"), "⾦", "金"),

    new Aelementstype(Set("25112511"), "⿰𠁣𠃛", "門"),
    new Aelementstype(Set("25112511"), "門", "門"),
    new Aelementstype(Set("25112511"), "⾨", "門"),

    new Aelementstype(Set("(12|21)11254444"), "⿹⑥灬", "馬"),
    new Aelementstype(Set("(12|21)11254444"), "馬", "馬"),
    new Aelementstype(Set("(12|21)11254444"), "⾺", "馬"),

    new Aelementstype(Set("34(1|4)(51154|511211)"), "⿱人⿱丶⑤", "食"),
    new Aelementstype(Set("34(1|4)(51154|511211)"), "食", "食"),
    new Aelementstype(Set("34(1|4)(51154|511211)"), "飠", "食"),
    new Aelementstype(Set("34(1|4)(51154|511211)"), "⾷", "食"),
    new Aelementstype(Set("34(1|4)(51154|511211)"), "⻝", "食"),
    new Aelementstype(Set("34(1|4)(51154|511211)"), "⻞", "食"),
    new Aelementstype(Set("34(1|4)(51154|511211)"), "⻟", "食"),

    new Aelementstype(Set("1251112"), "車", "車"),
    new Aelementstype(Set("1251112"), "⾞", "車"))

  val idsToStrokeMap: Map[String, Aelementstype] = elementTypes.map(x => x.ids -> x).toMap
}


