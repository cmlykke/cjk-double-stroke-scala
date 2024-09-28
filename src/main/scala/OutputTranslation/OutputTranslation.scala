package OutputTranslation

import ElementGenerator.{ElementAdjustedCodes, ElementTranslateToAlphabet}
import UtilityClasses.{CedictEntry, ConwayColl, ConwayUnambigous, Grapheme, OutputEntry, StaticFileCharInfoWithLetterConway}
import staticFileGenerators.Conway.GenerateConwayCodes
import staticFileGenerators.cedictMap.GenerateCedictMap

import scala.collection.mutable
import scala.collection.mutable.HashMap

type TranslationFunction = (List[Set[ConwayUnambigous]], String) => Set[String]

class OutputTranslation {
  
  def generateJundaGraphemeOrder(chineseStrGraphemes: Set[Grapheme]): List[Grapheme] = {
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
    chineseStrGraphemes.toList.sorted(ordering)
  }

  def generateTzaiGraphemeOrder(chineseStrGraphemes: Set[Grapheme]): List[Grapheme] = {
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
    chineseStrGraphemes.toList.sorted(ordering)
  }
/*
  def staticFileCharToOutputEntry(static: Set[StaticFileCharInfoWithLetterConway],
                                  translationFn: TranslationFunction): Set[OutputEntry] = {
    val res: mutable.Set[OutputEntry] = mutable.Set[OutputEntry]()
    for (charInfo <- static) {
      val info = charInfo.letterConway
      val test = ""
    }
    res.toSet
  }

  def staticFileInfoToOutputEntry(staticFileInfo: Set[StaticFileCharInfoWithLetterConway]): Set[OutputEntry] = {
    val res: mutable.Set[OutputEntry] = mutable.Set[OutputEntry]()
    for (eachInfo <- staticFileInfo) {
      val junda: List[Grapheme] = if (eachInfo.grapheme.junda.isDefined) List(eachInfo.grapheme) else List()
      val tzai: List[Grapheme] = if (eachInfo.grapheme.tzai.isDefined) List(eachInfo.grapheme) else List()
      val codes: Set[String] = eachInfo.letterConway.map(x => x.conwayPairs.mkString("")).toSet
      val entry: OutputEntry = new OutputEntry(
        eachInfo.grapheme.char,//ceEntry.chineseStr,
        "", //ceEntry.meaning,
        "", //ceEntry.pronounciation,
        "", //ceEntry.tradSimp,
        junda,
        tzai,
        codes
      )
      res.add(entry)
    }
    return res.toSet
  }
*/
  def cedictToOutputEntry(cedictEntries: Set[CedictEntry], 
                          translationFn: TranslationFunction): Set[OutputEntry] = {
    val res: mutable.Set[OutputEntry] = mutable.Set[OutputEntry]()
    for (ceEntry <- cedictEntries) {
      if (ceEntry.chineseStr == "井陘礦") {
        val tes: String = ""
      }
      val jundaReverseOrder: List[Grapheme] = generateJundaGraphemeOrder(ceEntry.chineseStrGraphemes).reverse
      val tzaiReverseOrder: List[Grapheme] = generateTzaiGraphemeOrder(ceEntry.chineseStrGraphemes).reverse
      val outputCodes: Set[String] = translationFn(ceEntry.unambigous, ceEntry.chineseStr)
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
      x.jundaReverseOrder.size == 1 &&
      x.jundaReverseOrder(0).junda.isDefined && 
      x.jundaReverseOrder(0).junda.get.ordinal <= 3000)
    return res
  }

  def getTzaiSingle3000(outputCedict: Set[OutputEntry]): Set[OutputEntry] = {
    val res: Set[OutputEntry] = outputCedict.filter(x =>
      x.tzaiReverseOrder.size == 1 &&
      x.tzaiReverseOrder(0).tzai.isDefined &&
      x.tzaiReverseOrder(0).tzai.get.ordinal <= 3000)
    return res
  }

  def getJundaMulti3000(outputCedict: Set[OutputEntry]): Set[OutputEntry] = {
    val res: Set[OutputEntry] = outputCedict.filter { x =>
      x.jundaReverseOrder.size > 1 &&
        x.jundaReverseOrder.forall(grapheme =>
          grapheme.junda.exists(_.ordinal <= 3000)
        )
    }
    res
  }

  def getTzaiMulti3000(outputCedict: Set[OutputEntry]): Set[OutputEntry] = {
    val res: Set[OutputEntry] = outputCedict.filter { x =>
      x.tzaiReverseOrder.size > 1 &&
      x.tzaiReverseOrder.forall(grapheme =>
        grapheme.tzai.exists(_.ordinal <= 3000)
      )
    }
    res
  }

  /*
  inputChineseStr: String,
                  inputMeaning: String,
                  inputPronounciation: String,
                  inputTradSimp: String,
                  inpjundaReverseOrder: List[Grapheme],
                  inptzaiReverseOrder: List[Grapheme],
                  inpcodes: Set[String]) {
  */
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
  val outputCedict: Set[OutputEntry] = outClass.cedictToOutputEntry(cedict, TranslationFunctions.translateVersionOne)
  //class CedictSubEntry(chineseStr: String,
  //                     inputGrap: Grapheme,
  //                     //maybeInfo: Option[StaticFileCharInfoWithLetterConway],
  //                     charMap: Map[Grapheme, StaticFileCharInfoWithLetterConway]) {
  //val conwayWithElems: Set[CedictSubEntry] = outClass.generateSubentriesFromConway(conwayMap)
  //val outputConway: Set[OutputEntry] = outClass.staticFileCharToOutputEntry(conwayWithElems, TranslationFunctions.translateVersionOne)

  val cedictMap: Map[String, CedictEntry] = GenerateCedictMap.cedictMap
  val conway: Map[Grapheme, StaticFileCharInfoWithLetterConway] = ElementTranslateToAlphabet.completeTranslatedConwayMap
  val conwayOutFull: Set[OutputEntry] = outClass.getConwayFull(conway.values.toList, cedictMap)
  //create the function below
  //val outputConway: Set[OutputEntry] = outClass.conwayToOutputEntrySanCedict(conway, cedict)
  //val conwayInfo: Set[StaticFileCharInfoWithLetterConway] = ElementAdjustedCodes.elemAdjustedAllChars

  //val outputConway: Set[OutputEntry] = outClass.staticFileInfoToOutputEntry(conwayInfo)
  val jundaSingelOut: Set[OutputEntry] = outClass.getJundaSingle3000(outputCedict)
  val jundaMultiOut: Set[OutputEntry] = outClass.getJundaMulti3000(outputCedict)
  val tzaiSingelOut: Set[OutputEntry] = outClass.getTzaiSingle3000(outputCedict)
  val tzaiMultiOut: Set[OutputEntry] = outClass.getTzaiMulti3000(outputCedict)

}
