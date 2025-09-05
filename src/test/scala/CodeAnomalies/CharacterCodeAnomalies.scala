package CodeAnomalies

import GenerateOutput.GenerateOutputStrings
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import UtilityClasses.{Grapheme, OutputEntry}
import UtilityClasses.InputSizes.Three_oneAndFive_one
import UtilityClasses.{CharSystem, Conway, ConwayColl, ConwayUnambigous, Grapheme, StaticFileCharInfo}
import ElementGenerator.ElementList
import OutputTranslation.OutputSorting


class CharacterCodeAnomalies extends AnyFlatSpec with Matchers {

  val generate = new GenerateOutputStrings()
  val outputLinesJ: List[String] = generate.generateWithSpecial(GenerateOutputStrings.mapFullJunda)
  val outJStr: Set[String] = outputLinesJ
    .map(x => x.split("\t"))
    .map(x => x(1)).toSet
  val outputLinesT: List[String] = generate.generateWithSpecial(GenerateOutputStrings.mapFullTzai)
  val outTStr: Set[String] = outputLinesT
    .map(x => x.split("\t"))
    .map(x => x(1)).toSet

  // 1200 character are not in the current (2025-09-04) liuma output files. They are all rare, but should eventually be included.
  it should "character not included in liuma output" in {

    val conwaySet: Set[OutputEntry] = OutputSorting.conFull

    val singleJunda = outJStr
      .filter(z => Grapheme.isGrapheme(z)).toSet
    val singleTzai = outTStr
      .filter(z => Grapheme.isGrapheme(z)).toSet
    //It contains 29.512 different single characters, and 179.799 multi-character words

    val conwaysetPureString = conwaySet.map(x => x.chineseStr).toSet

    val singlejundamissing = singleJunda.filter(eachChar => !conwaysetPureString.contains(eachChar))
    val singletzaimissing = singleTzai.filter(eachChar => !conwaysetPureString.contains(eachChar))

    singleJunda.size shouldBe 29512
    singleTzai.size shouldBe 29512
    conwaySet.size shouldBe 28312
    singlejundamissing.size shouldBe 1200
    singletzaimissing.size shouldBe 1200
  }

  it should "code for unusual characeters" in {
    val conwaySet: Set[OutputEntry] = OutputSorting.conFull

    val unusualList: List[String] = List(
      "置", "具", "州", "洲", "本",
      "狗", "連", "這", "肺", "它",
      "次", "翔", "忙", "來", "麥",
      "跑", "攪", "決", "躁"
    )

    val setup = conwaySet.filter(x => x.chineseStr == "置").head // xbhugh xbhh xbhbgm xbhm xbhbgh xbhugm
    setup.codes shouldBe Set("xbhugh", "xbhh", "xbhbgm", "xbhm", "xbhbgh", "xbhugm")
    val tool = conwaySet.filter(x => x.chineseStr == "具").head  // xhhozz  xhho
    tool.codes shouldBe Set("xhhozz",  "xhho")
    val state = conwaySet.filter(x => x.chineseStr == "州").head // err  errzzz  errz
    state.codes shouldBe Set("err",  "errzzz",  "errz")
    val continent = conwaySet.filter(x => x.chineseStr == "洲").head  //wlocnz  wlor
    continent.codes shouldBe Set("wlocnz",  "wlor")
    val notebook = conwaySet.filter(x => x.chineseStr == "本").head   // joh  johzzz  johz
    notebook.codes shouldBe Set("dhzz", "johzzz", "dh") // Set("joh",  "johzzz",  "johz")
    
    val dog = conwaySet.filter(x => x.chineseStr == "狗").head       // pifgzz  difgzz  pifg  difg
    dog.codes shouldBe Set( "pifgzz",  "difgzz",  "pifg",  "difg")
    val oneafteranother = conwaySet.filter(x => x.chineseStr == "連").head   // jghcat  jghcqt  jghcsz  jghs
    oneafteranother.codes shouldBe Set("jghcat",  "jghcqt",  "jghcsz",  "jghs")
    val thisthis = conwaySet.filter(x => x.chineseStr == "這").head    // hhxlat  iqtz  iws  iwsz  hhxlsz  iqsz  thxlqt  thxlat  iqt  iqs  hhxlqt  thxlsz
    thisthis.codes shouldBe Set("hhxlat",  "iqtz",  "iws",  "iwsz",  "hhxlsz",  "iqsz",  "thxlqt",  "thxlat",  "iqt",  "iqs",  "hhxlqt",  "thxlsz")
    val lungs = conwaySet.filter(x => x.chineseStr == "肺").head   //  phjfzz  ptjfzz  ptjf  phjf
    lungs.codes shouldBe Set("phjfzz",  "ptjfzz",  "ptjf",  "phjf")
    val itit = conwaySet.filter(x => x.chineseStr == "它").head   //  wdgzzz  wayzzz  wdg  way  wayz  wdgz
    itit.codes shouldBe Set("wdgzzz",  "wayzzz",  "wdg",  "way",  "wayz",  "wdgz")
    
    val nextnext = conwaySet.filter(x => x.chineseStr == "次").head  //  tpoz  hpo  hpozzz  tpozzz  tpo  hpoz
    nextnext.codes shouldBe Set("tpoz",  "hpo",  "hpozzz",  "tpozzz",  "tpo",  "hpoz")
    val soar = conwaySet.filter(x => x.chineseStr == "翔").head     // ehksmt  ehkdpi  ehki  ehkt
    soar.codes shouldBe Set("ehksmt",  "ehkdpi",  "ehki",  "ehkt")
    val busy = conwaySet.filter(x => x.chineseStr == "忙").head   // rwmz rwm rwmzzz cwmz wcmzzz wcm cwm wcmz cwmzzz
    busy.codes shouldBe Set( "rwmz", "rwm", "rwmzzz", "cwmz", "wcmzzz", "wcm", "cwm", "wcmz", "cwmzzz")
    val come = conwaySet.filter(x => x.chineseStr == "來").head   // doo jooozz dooz
    come.codes shouldBe Set("doo", "jooozz", "dooz")
    val wheat = conwaySet.filter(x => x.chineseStr == "麥").head  // jooopt doos
    wheat.codes shouldBe Set("jooopt", "doos")
    
    val runrun = conwaySet.filter(x => x.chineseStr == "跑").head  // xjmpgg xjjkam jpgg
    runrun.codes shouldBe Set("xjmpgg", "xjjkam", "jpgg")
    val stirstir = conwaySet.filter(x => x.chineseStr == "攪").head  // jkeenp loop
    stirstir.codes shouldBe Set( "jkeenp", "loop")
    val decide = conwaySet.filter(x => x.chineseStr == "決").head   // wmktzz  wmkt
    decide.codes shouldBe Set( "wmktzz", "wmkt")
    val impetous = conwaySet.filter(x => x.chineseStr == "躁").head  // xjmxjo  xjjjgo  jxjo
    impetous.codes shouldBe Set("xjmxjo",  "xjjjgo",  "jxjo")
    




    conwaySet.size shouldBe 28312
  }

}
