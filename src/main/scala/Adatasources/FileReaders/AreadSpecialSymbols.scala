package Adatasources.FileReaders

import scala.io.Source

object AreadSpecialSymbols {

  val OnepuncPath: String = "src/main/scala/staticFileGenerators/SpecialCharacters/punctuation.txt"
  
  def getLinesFromPunctuation(): List[String] = {
    val bufferedSource = Source.fromFile(OnepuncPath)
    val lines: List[String] = bufferedSource.getLines.toList
    lines
  }
}


//val OnepuncPath: String = "src/main/scala/staticFileGenerators/SpecialCharacters/punctuation.txt"
