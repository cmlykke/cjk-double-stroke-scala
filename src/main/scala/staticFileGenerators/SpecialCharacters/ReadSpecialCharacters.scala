package staticFileGenerators.SpecialCharacters

import UtilityClasses.{Grapheme, OutputEntry}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.Source

class ReadSpecialCharacters {
  
  //covered uncode blocks
  // Geometric Shapes U+25A0..U+25FF
  // CJK Radicals Supplement U+2E80..U+2EFF
  // Kangxi Radicals U+2F00..U+2FDF
  // Ideographic Description Characters U+2FF0..U+2FFF
  // CJK Symbols and Punctuation U+3000..U+303F
  // Bopomofo U+3100..U+312F, U+31A0..U+31BF
  // CJK Strokes U+31C0..U+31EF
  // Enclosed CJK Letters and Months U+3200..U+32FF
  // CJK Compatibility U+3300..U+33FF, U+FE30..U+FE4F
  val OnepuncPath: String = "src/main/scala/staticFileGenerators/SpecialCharacters/punctuation.txt"
  val TwogeometricPath: String = "src/main/scala/staticFileGenerators/SpecialCharacters/geometric.txt"
  val ThreeradicalsPath: String = "src/main/scala/staticFileGenerators/SpecialCharacters/radicals.txt"
  val FourdescriptionPath: String = "src/main/scala/staticFileGenerators/SpecialCharacters/description.txt"
  val FivesymbolsPath: String = "src/main/scala/staticFileGenerators/SpecialCharacters/symbols.txt"
  val SixbopomofoPath: String = "src/main/scala/staticFileGenerators/SpecialCharacters/bopomofo.txt"
  val SevenstrokesPath: String = "src/main/scala/staticFileGenerators/SpecialCharacters/strokes.txt"
  val EightenclosedPath: String = "src/main/scala/staticFileGenerators/SpecialCharacters/enclosed.txt"
  val NinecompatibilityPath: String = "src/main/scala/staticFileGenerators/SpecialCharacters/compatibility.txt"


  def readLinesFromAllFiles(test: List[String]): List[String] = {
    if (test.size != 9) {
      throw new Exception("special character list size is not as expected (9)")
    }
    var res: ListBuffer[String] = ListBuffer[String]()
    val one: List[String] = readLinesFromFile(test(0))
    val two: List[String] = readLinesFromFile(test(1))
    val three: List[String] = readLinesFromFile(test(2))
    val four: List[String] = readLinesFromFile(test(3))
    val five: List[String] = readLinesFromFile(test(4))
    val six: List[String] = readLinesFromFile(test(5))
    val seven: List[String] = readLinesFromFile(test(6))
    val eight: List[String] = readLinesFromFile(test(7))
    val nine: List[String] = readLinesFromFile(test(8))
    res.appendAll(one)
    res.appendAll(two)
    res.appendAll(three)
    res.appendAll(four)
    res.appendAll(five)
    res.appendAll(six)
    res.appendAll(seven)
    res.appendAll(eight)
    res.appendAll(nine)
    res.toList
  }
  
  private def readLinesFromFile(path: String): List[String] = {
    val bufferedSource = Source.fromFile(path)
    val lines: List[String] = bufferedSource.getLines.toList
    lines
  }
  
  private def readFromFile(filepath: String): Set[Grapheme] = {
    var res: mutable.Set[Grapheme] = mutable.Set[Grapheme]()
    val bufferedSource = Source.fromFile(filepath)
    val lines: List[String] = bufferedSource.getLines.toList
    for (eachLine <- lines) {
      try {
        val splitEach = eachLine.split("\\s").toList
        val grap: Set[Grapheme] = Grapheme.splitIntoGraphemes(splitEach(0))
          .map(x => Grapheme(x)).toSet
        res.addAll(grap)
      } catch {
        case e: Throwable =>
          val test = ""
      }
    }
    res.toSet
  }

  def generateOutputFromLines(lines: List[String]): List[OutputEntry] = {
    /*
    (inputChineseStr: String,
                  inputMeaning: String,
                  inputPronounciation: String,
                  inputTradSimp: String,
                  inpjundaReverseOrder: List[Grapheme],
                  inptzaiReverseOrder: List[Grapheme],
                  inpcodes: Set[String])
    */
    var res: ListBuffer[OutputEntry] = new ListBuffer[OutputEntry]()
    for (line <- lines) {
      val splitLine: List[String] = line.split("\\t+").toList
      if (splitLine.size != 2) {
        val test = ""
        throw new Exception("special character line is not correct")
        if (isOnlyWhitespace(splitLine(0)) && splitLine(0).size != 1) {
          throw new Exception("more than one whitespace")
        }
      }
      val out: OutputEntry = new OutputEntry(
        splitLine(0), "", "", "", List(), List(), Set(splitLine(1)))
      res.append(out)
    }
    return res.toList
  }

  def isOnlyWhitespace(s: String): Boolean = {
    s.forall(Character.isWhitespace)
  }
}

object ReadSpecialCharacters {
  private val inst = new ReadSpecialCharacters()
  val allLines: List[String] = inst.readLinesFromAllFiles(List(
    inst.OnepuncPath, inst.TwogeometricPath, inst.ThreeradicalsPath, inst.FourdescriptionPath,
    inst.FivesymbolsPath, inst.SixbopomofoPath, inst.SevenstrokesPath, inst.EightenclosedPath,
    inst.NinecompatibilityPath))
  val allCharacterOutput: List[OutputEntry] = inst.generateOutputFromLines(allLines)
}
