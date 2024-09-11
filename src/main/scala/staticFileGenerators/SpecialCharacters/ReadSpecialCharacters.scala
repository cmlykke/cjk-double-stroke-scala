package staticFileGenerators.SpecialCharacters

import UtilityClasses.Grapheme

import scala.collection.mutable
import scala.io.Source

class ReadSpecialCharacters {

  val puncPath: String = "src/main/scala/staticFileGenerators/SpecialCharacters/punctuation.txt"
  val specialPath: String = "src/main/scala/staticFileGenerators/SpecialCharacters/specialcharacters.txt"

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

  def specialLines(): String = {
    val bufferedSourcePunc = Source.fromFile(puncPath)
    val linesPunc: List[String] = bufferedSourcePunc.getLines.toList
    val bufferedSourceSpecial = Source.fromFile(specialPath)
    val linesSpecial: List[String] = bufferedSourceSpecial.getLines.toList

    val puncStr: String = linesPunc.mkString("\n")
    val specialStr: String = linesSpecial.mkString("\n")
    puncStr + "\n" + specialStr
  }

  def punctuationSet(): Set[Grapheme] = {
    val res = readFromFile(puncPath)
    res
  }

  def specialCharacterSet(): Set[Grapheme] = {
    val res = readFromFile(specialPath)
    res
  }

}

object ReadSpecialCharacters {
  private val inst = new ReadSpecialCharacters()
  val allCharacterStr: String = inst.specialLines()
  val punctiation: Set[Grapheme] = inst.punctuationSet()
  val specialCharacters: Set[Grapheme] = inst.specialCharacterSet()
}
