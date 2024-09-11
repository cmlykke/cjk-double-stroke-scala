package staticFileGenerators.Conway
import UtilityClasses.{ConwayColl, Grapheme}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths, StandardOpenOption}
import scala.collection.mutable
import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer
import scala.io.Source
//import scala.collection.mutable.Set

class ReadConwayData {
  
  def fileToNonAsciiGraphemeSet(filepath: String): Set[Grapheme] = {
    Grapheme.splitIntoGraphemes(Source.fromFile(filepath).mkString(""))
      .filter(x => x.head.toInt > 127)
      .map(x => Grapheme(x)).toSet    
  }

  def writeConwayToFile(filePath: String,
                        content: HashMap[Grapheme, ConwayColl]): Unit = {
    val conwayLi: List[ConwayColl] = content.values.toList.sortBy(_.unicode)
    val lineStr: List[String] = conwayLi.map { x =>
      s"${x.unicode}\t${x.char.char}\t${x.rawConway.rawConway}"
    }
    val result: String = lineStr.mkString("\n")
    val path = Paths.get(filePath)
    Files.write(path,
      result.getBytes(StandardCharsets.UTF_8),
      StandardOpenOption.CREATE,
      StandardOpenOption.TRUNCATE_EXISTING
    )
  }

  def mapConwayData(filePath: String): HashMap[Grapheme, ConwayColl] = {
    val bufferedSource = Source.fromFile(filePath)
    val lines = bufferedSource.getLines
    var resultMap = new HashMap[Grapheme, ConwayColl]()

    //var setOfChars: Set[String] = Set()
    //var allLines = ListBuffer[String]()
    //allLines.addAll(lines)
    //allLines.addAll(missingCedictBufferedSource)

    for (line <- lines) {
      //val processedLine = if (line.startsWith("\ufeff")) line.substring(1) else line
      if (line.startsWith("U+")) {
        val splitLine = line.split("\\s")
        val field1 = splitLine(0)
        val field2raw = splitLine(1)
        val field2 = field2raw.replaceAll("[\\p{ASCII}]", "")

        if (Grapheme.isGrapheme(field2)) {
          val restOfTheFields: List[String] = splitLine.drop(2).toList
          resultMap.put(Grapheme(field2), ConwayColl(restOfTheFields, field1, Grapheme(field2)))
        } else {
          print(field1)
        }
      }
    }
    bufferedSource.close()

    val conwayGraph: Set[Grapheme] = resultMap.keys.toSet
    resultMap
  }

  def isAscii(c: Char): Boolean = c <= 127//1000//127

  def isAsciiString(str: String): Boolean = str.forall(isAscii)

  def stringsToUnicodeChars(strings: List[String]): List[String] = {
    strings.flatMap { str =>
      str.codePoints().toArray.map { codePoint =>
        new String(Character.toChars(codePoint))
      }
    }
  }

}

/*
    val heisigBufferedSource = Source.fromFile(heisigFilePath)("UTF-8")
    val heisigLines = heisigBufferedSource.getLines().toList
    heisigBufferedSource.close()
    val unicodeChars: Set[String] = stringsToUnicodeChars(heisigLines).toSet

    var mutableHeisig = ListBuffer[String]()
    for (heisigCHR <- unicodeChars) {
      val heisigGra = Grapheme(heisigCHR)
      if (!conwayGraph.contains(heisigGra) && !isAsciiString(heisigCHR)) {
        mutableHeisig.addOne(heisigGra.char)
      }
    }*/
//unicodeChars.foreach()

//junda -- 8220, 9019  裏  秊
//tzai -- 4782, 9574  兀  嗀
//class ConwayColl(input: List[String], unicodeInput: String, charInput: Grapheme) {
//add chars from junda and tzai that are missing from conway

/*

resultMap.put(Grapheme("裏"), ConwayColl(List("4125111213534"), "U+F9E7", Grapheme("裏")))
resultMap.put(Grapheme("秊"), ConwayColl(List("31234312"), "U+F995", Grapheme("秊")))
resultMap.put(Grapheme("兀"), ConwayColl(List("135"), "U+FA0C", Grapheme("兀")))
resultMap.put(Grapheme("嗀"), ConwayColl(List("1214512513554"), "U+FA0D", Grapheme("嗀")))
//heisig characters that are missing from conway: 𫍟 2718 prevarication
resultMap.put(Grapheme("𫍟"), ConwayColl(List("4531525"), "U+2E8F", Grapheme("𫍟")))

//CJK elements that doesnt have a code
resultMap.put(Grapheme("⺮"), ConwayColl(List("314314"), "U+2EAE", Grapheme("⺮"))) //'⺮' 11950
resultMap.put(Grapheme("々"), ConwayColl(List("354"), "U+3005", Grapheme("々"))) //'々' 12293
resultMap.put(Grapheme("〡"), ConwayColl(List("2"), "U+3021", Grapheme("〡"))) //〡 U+3021
resultMap.put(Grapheme("〢"), ConwayColl(List("22"), "U+3022", Grapheme("〢"))) //〢 U+3022
resultMap.put(Grapheme("〣"), ConwayColl(List("222"), "U+3023", Grapheme("〣"))) // 〣 U+3023
resultMap.put(Grapheme("〤"), ConwayColl(List("34"), "U+3024", Grapheme("〤"))) // 〤 U+3024
resultMap.put(Grapheme("〥"), ConwayColl(List("5"), "U+3025", Grapheme("〥"))) // 〥 U+3025
resultMap.put(Grapheme("〦"), ConwayColl(List("21"), "U+3026", Grapheme("〦"))) // 〦 U+3026
resultMap.put(Grapheme("〧"), ConwayColl(List("211"), "U+3027", Grapheme("〧"))) // 〧 U+3027
resultMap.put(Grapheme("〨"), ConwayColl(List("2111"), "U+3028", Grapheme("〨"))) // 〨 U+3028
resultMap.put(Grapheme("〩"), ConwayColl(List("(3134|54)"), "U+3029", Grapheme("〩"))) // 〩 U+3029
resultMap.put(Grapheme("〸"), ConwayColl(List("12"), "U+3038", Grapheme("〸"))) // 〸 U+3038
resultMap.put(Grapheme("〻"), ConwayColl(List("5"), "U+303B", Grapheme("〻"))) // 〻 U+303B
resultMap.put(Grapheme("ㄅ"), ConwayColl(List("5"), "U+3105", Grapheme("ㄅ"))) // ㄅ U+3105
resultMap.put(Grapheme("ㄧ"), ConwayColl(List("(1|2)"), "U+3127", Grapheme("ㄧ"))) // ㄧ U+3127
resultMap.put(Grapheme("ㄤ"), ConwayColl(List("135"), "U+3124", Grapheme("ㄤ"))) // ㄤ U+3124


//missing rare characters
//japanese era names
resultMap.put(Grapheme("㍻"), ConwayColl(List("1(34|43)12135(534|543)"), "U+337B", Grapheme("㍻"))) // ㍻ U+337B //平 1(34|43)12  成 135(534|543)
resultMap.put(Grapheme("㍼"), ConwayColl(List("25115325131234251"), "U+337C", Grapheme("㍼"))) // ㍼ U+337C 昭 251153251  和  31234251
resultMap.put(Grapheme("㍽"), ConwayColl(List("13412121"), "U+337D", Grapheme("㍽"))) // ㍽ U+337D  大  正
resultMap.put(Grapheme("㍾"), ConwayColl(List("2511351144154251"), "U+337E", Grapheme("㍾"))) // ㍾ U+337E  明 25113511  治 44154251

// other missing chars
resultMap.put(Grapheme("𪨊"), ConwayColl(List("5133434"), "U+2AA0A", Grapheme("𪨊"))) //“𪨊” (U+2AA0A)
resultMap.put(Grapheme("𪢌"), ConwayColl(List("251(245|425)125431234"), "U+2A88C", Grapheme("𪢌"))) // 𪢌 “𪢌” (U+2A88C)
resultMap.put(Grapheme("𬸩"), ConwayColl(List("32543123413435451"), "U+2CE29", Grapheme("𬸩"))) // “𬸩” (U+2CE29)
resultMap.put(Grapheme("𠇹"), ConwayColl(List("3254121"), "U+201F9", Grapheme("𠇹"))) // “𠇹” (U+201F9)
resultMap.put(Grapheme("𠮶"), ConwayColl(List("251342"), "U+20BB6", Grapheme("𠮶"))) // "𠮶" (U+20BB6)
resultMap.put(Grapheme("𠯠"), ConwayColl(List("2514354"), "U+20BE0", Grapheme("𠯠"))) // "𠯠" (U+20BE0)
resultMap.put(Grapheme("𪢠"), ConwayColl(List("251(122|1212|2112)(245|425)125431234"), "U+2A8A0", Grapheme("𪢠")))  // "𪢠" (U+2A8A0)
//resultMap.put(Grapheme(""), ConwayColl(List(""), "", Grapheme("")))  // 
//(122|1212|2112)35(1|4) 芍

 */

//resultMap.put(Grapheme(""), ConwayColl(List(""), "", Grapheme("")))
//output
