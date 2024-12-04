package OutputTranslation

import ElementGenerator.{ElementAdjustedCodes, ElementTranslateToAlphabet}
import UtilityClasses.{CedictEntry, ConwayColl, ConwayUnambigous, Grapheme, OutputEntry, StaticFileCharInfoWithLetterConway}
import staticFileGenerators.Conway.GenerateConwayCodes
import staticFileGenerators.cedictMap.GenerateCedictMap

import scala.collection.mutable
import scala.collection.mutable.{HashMap, ListBuffer}

type TranslationFunction = (List[Set[ConwayUnambigous]], String) => Set[String]
type TranslationFunctionCedict = (CedictEntry, List[Set[ConwayUnambigous]], String) => Set[String]

class OutputTranslation {
  
  def generateJundaGraphemeOrder(chineseStrGraphemes: List[Grapheme]): List[Grapheme] = {
    // Define an ordering based on the required sorting criteria
    val ordering: Ordering[Grapheme] = Ordering.by(grapheme => {
      def extractHexValue(hexString: String): Int = {
        Integer.parseInt(hexString.stripPrefix("U+"), 16)
      }

      // Ensure we use a very low frequency if not present to avoid being placed before entries with data
      val jundaFreq = -grapheme.junda.map(_.frequency).getOrElse(Double.MinValue)
      val tzaiFreq = -grapheme.tzai.map(_.frequency).getOrElse(Double.MinValue)
      val unicodeLen = grapheme.unicode.length
      val lastHexValue = grapheme.unicode.lastOption.map(extractHexValue).getOrElse(Int.MaxValue)
      (jundaFreq, tzaiFreq, unicodeLen, lastHexValue)
    })
    // Convert the set to a list and sort using the defined ordering
    chineseStrGraphemes.toSet.toList.sorted(ordering)
  }

  def generateTzaiGraphemeOrder(chineseStrGraphemes: List[Grapheme]): List[Grapheme] = {
    // Define an ordering based on the required sorting criteria
    val ordering: Ordering[Grapheme] = Ordering.by(grapheme => {
      def extractHexValue(hexString: String): Int = {
        Integer.parseInt(hexString.stripPrefix("U+"), 16)
      }

      // Ensure we use a very low frequency if not present to avoid being placed before entries with data
      val tzaiFreq = -grapheme.tzai.map(_.frequency).getOrElse(Double.MinValue)
      val jundaFreq = -grapheme.junda.map(_.frequency).getOrElse(Double.MinValue)
      val unicodeLen = grapheme.unicode.length
      val lastHexValue = grapheme.unicode.lastOption.map(extractHexValue).getOrElse(Int.MaxValue)
      (tzaiFreq, jundaFreq, unicodeLen, lastHexValue)
    })
    // Convert the set to a list and sort using the defined ordering
    chineseStrGraphemes.toSet.toList.sorted(ordering)
  }
  
  def conwayToOutputEntry(conwayallch: Set[StaticFileCharInfoWithLetterConway]): Set[OutputEntry] = {
    val res: mutable.Set[OutputEntry] = mutable.Set[OutputEntry]()
    for (conEntry <- conwayallch) {
      val outputCodes: Set[String] =  ConwayToOutput.rawConwayToOutputCodes(List(conEntry.grapheme))
      val entry: OutputEntry = new OutputEntry(
        conEntry.grapheme.char,
        "",
        "",
        "",
        List(conEntry.grapheme),
        List(conEntry.grapheme),
        outputCodes
      )
      res.add(entry)
    }
    //val test = res.filter(x => x.chineseStr == "糸").toSet;

    res.toSet
  } 
  
  def cedictToOutputEntry(cedictEntries: Set[CedictEntry]): Set[OutputEntry] = {
    val res: mutable.Set[OutputEntry] = mutable.Set[OutputEntry]()
    for (ceEntry <- cedictEntries) {
      if (ceEntry.chineseStr == "仫") {
        val tes: String = ""
      }
      val jundaReverseOrder: List[Grapheme] = generateJundaGraphemeOrder(ceEntry.chineseStrGraphemes).reverse
      val tzaiReverseOrder: List[Grapheme] = generateTzaiGraphemeOrder(ceEntry.chineseStrGraphemes).reverse
      val outputCodes: Set[String] =  ConwayToOutput.rawConwayToOutputCodes(ceEntry.chineseStrGraphemes)
      val entry: OutputEntry = new OutputEntry(
        ceEntry.chineseStr,
        ceEntry.meaning,
        ceEntry.pronounciation,
        ceEntry.tradSimp,
        jundaReverseOrder,
        tzaiReverseOrder,
        outputCodes
      )
      res.add(entry)
    }
    res.toSet
  }
  
  def getJundaSingle3000(outputCedict: Set[OutputEntry]): Set[OutputEntry] = {
    val res: Set[OutputEntry] = outputCedict.filter(x => 
      x.jundaReverseOrderG.size == 1 &&
      x.jundaReverseOrderG(0).junda.isDefined && 
      x.jundaReverseOrderG(0).junda.get.ordinal <= 3000)
    return res
  }

  def getTzaiSingle3000(outputCedict: Set[OutputEntry]): Set[OutputEntry] = {
    val res: Set[OutputEntry] = outputCedict.filter(x =>
      x.tzaiReverseOrderG.size == 1 &&
      x.tzaiReverseOrderG(0).tzai.isDefined &&
      x.tzaiReverseOrderG(0).tzai.get.ordinal <= 3000)
    return res
  }

  def getJundaMulti3000(outputCedict: Set[OutputEntry]): Set[OutputEntry] = {
    val res: Set[OutputEntry] = outputCedict.filter { x =>
      x.jundaReverseOrderG.size > 1 &&
        x.jundaReverseOrderG.forall(grapheme =>
          grapheme.junda.exists(_.ordinal <= 3000)
        )
    }
    res
  }

  def getTzaiMulti3000(outputCedict: Set[OutputEntry]): Set[OutputEntry] = {
    val res: Set[OutputEntry] = outputCedict.filter { x =>
      x.tzaiReverseOrderG.size > 1 &&
      x.tzaiReverseOrderG.forall(grapheme =>
        grapheme.tzai.exists(_.ordinal <= 3000)
      )
    }
    res
  }

  def getConwayFull(conwayList: List[StaticFileCharInfoWithLetterConway],
                         cedictMap: Map[String, CedictEntry]): Set[OutputEntry] = {
    var res: mutable.Set[OutputEntry] = mutable.Set[OutputEntry]()
    for (conway <- conwayList) {
      //if (!cedictMap.contains(conway.grapheme.char)) {
        val jundaList: List[Grapheme] = if (conway.grapheme.junda.isDefined) List(conway.grapheme) else List()
        val tzaiList: List[Grapheme] = if (conway.grapheme.tzai.isDefined) List(conway.grapheme) else List()
        val inpCodes: Set[String] = conway.letterConway.map(x => x.conwayPairs.mkString("")).toSet
        val output: OutputEntry = new OutputEntry(
          conway.grapheme.char, "", "", "", jundaList, tzaiList, inpCodes)
        res.add(output)
      //}
    }
    return res.toSet
  }
  
  def getConwaySanCedict(conwayList: List[StaticFileCharInfoWithLetterConway],
                         cedictMap: Map[String, CedictEntry]): Set[OutputEntry] = {
    var res: mutable.Set[OutputEntry] = mutable.Set[OutputEntry]()
    for (conway <- conwayList) {
      if (!cedictMap.contains(conway.grapheme.char)) {
        val jundaList: List[Grapheme] = if (conway.grapheme.junda.isDefined) List(conway.grapheme) else List()
        val tzaiList: List[Grapheme] = if (conway.grapheme.tzai.isDefined) List(conway.grapheme) else List()
        val inpCodes: Set[String] = conway.letterConway.map(x => x.conwayPairs.mkString("")).toSet
        val output: OutputEntry = new OutputEntry(
          conway.grapheme.char, "", "", "", jundaList, tzaiList, inpCodes)
        res.add(output)
      }
    }
    return res.toSet
  }
}

object OutputTranslation {
  val outClass = new OutputTranslation()
  val cedict: Set[CedictEntry] = GenerateCedictMap.cedictCompleteSet
  val conwayallch: Set[StaticFileCharInfoWithLetterConway] = ElementTranslateToAlphabet.generateTranslatedAllChars()
  val outputCedict: Set[OutputEntry] = outClass.cedictToOutputEntry(cedict)
  val outputConway: Set[OutputEntry] = outClass.conwayToOutputEntry(conwayallch)
  
  val jundaSingelOut: Set[OutputEntry] = outClass.getJundaSingle3000(outputCedict)
  val jundaMultiOut: Set[OutputEntry] = outClass.getJundaMulti3000(outputCedict)
  val tzaiSingelOut: Set[OutputEntry] = outClass.getTzaiSingle3000(outputCedict)
  val tzaiMultiOut: Set[OutputEntry] = outClass.getTzaiMulti3000(outputCedict)

}
