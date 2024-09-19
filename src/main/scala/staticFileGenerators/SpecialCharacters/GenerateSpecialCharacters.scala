package staticFileGenerators.SpecialCharacters

import java.io.IOException
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths
import scala.collection.mutable.ListBuffer


class GenerateSpecialCharacters {

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

  def generateShapes(initial: String, outputPath: String, ranges: List[(String, String)]): Unit = {
    val localTwoLetterCombis: List[String] = GenerateSpecialCharacters.twoletterCombi
    var unicodeRangeResults: ListBuffer[String] = new ListBuffer[String]()
    for (eachRange <- ranges) {
      val currentRange: List[String] = generateUnicodeRange(eachRange)
      unicodeRangeResults.addAll(currentRange)
    }
    if (unicodeRangeResults.size > localTwoLetterCombis.size) {
      throw new Exception("unicode range too great for two letter combos")
    }
    val output: String = mergeLists(initial, unicodeRangeResults.toList, localTwoLetterCombis)
    val path = Paths.get(outputPath)
    Files.write(path, output.getBytes(StandardCharsets.UTF_8))
    val test = ""
  }

  private def mergeLists(initial: String, list1: List[String], list2: List[String]): String = {
    // Ensure that list1 and list2 have the same size up to the size of list1
    val merged = list1.indices.map { index =>
      s"${list1(index)}"+"\t"+initial+s"${list2(index)}"
    }
    // Join the merged lines with newline character
    merged.mkString("\n")
  }

  def generateLetterCombinations(): List[String] = {
    val letters = "qwertyuiopasdfghjklzxcvbnm"
    val combinations = for {
      first <- letters
      second <- letters
    } yield s"$first$second"

    combinations.flatMap(combination => List.fill(9)(combination)).toList
  }

  private def generateUnicodeRange(range: (String, String)): List[String] = {
    // Extract the start and end codes from the tuple
    val (startCode, endCode) = range

    // Convert the hexadecimal string with "U+" prefix to integer
    val start = Integer.parseInt(startCode.stripPrefix("U+"), 16)
    val end = Integer.parseInt(endCode.stripPrefix("U+"), 16)

    // Generate the list of Unicode characters for the given range
    (start to end).map(codePoint => new String(Character.toChars(codePoint))).toList
  }

  def generatePunctuation(pathStr: String): Unit = {
    val output: String = """。	.
                         |.	.
                         |，	,
                         |,	,
                         |、	,
                         |　	z
                         |　	za
                         |〃	zs
                         |“”	zd
                         |‘’	zf
                         |（）	zg
                         |？	zh
                         |！	zj
                         |：	zk
                         |；	zl
                         |〇	zx
                         |［］    zx
                         |《》	zx
                         |「 」 zx
                         |……  zx
                         |·	zx
                         |〻 zx
                         |〃 zx
                         |々 zx
                         |〇	zxa
                         |［］    zxs
                         |《》	zxd
                         |「 」 zxf
                         |……  zxg
                         |·	zxh
                         |〻 zxj
                         |〃 zxk
                         |々 zxl                        
                         |⸺	zz
                         |–	zz
                         |……  zz
                         |⋯	zz
                         |·	zz
                         |‧   zz
                         |〜  zz
                         |～	zz
                         |〰	zz
                         |⸺	zza
                         |–	zzs
                         |……  zzd
                         |⋯	zzf
                         |·	zzg
                         |‧   zzh
                         |〜  zzj
                         |～	zzk
                         |〰	zzl
                         |［］    zzz
                         |【】	zzz
                         |〖〗	zzz
                         |〈〉	zzz
                         |《》	zzz
                         |「」	zzz
                         |『』	zzz
                         |〔〕	zzz
                         |﹏﹏	zzz
                         |［］    zzza
                         |【】	zzzs
                         |〖〗	zzzd
                         |〈〉	zzzf
                         |《》	zzzg
                         |「」	zzzh
                         |『』	zzzj
                         |〔〕	zzzk
                         |﹏﹏	zzzl
                         |〇	zzzz
                         |◯	zzzz
                         |○	zzzz
                         |●	zzzz
                         |◎	zzzz
                         |⭕️	zzzz
                         |✗	zzzz
                         |×	zzzz
                         |△	zzzz
                         |〇	zzzza
                         |◯	zzzzs
                         |○	zzzzd
                         |●	zzzzf
                         |◎	zzzzg
                         |⭕️	zzzzh
                         |✗	zzzzj
                         |×	zzzzk
                         |△	zzzzl
                         | 	zzzzx
                         |.	zzzzx
                         |,	zzzzx
                         |:	zzzzx
                         |;	zzzzx
                         |"	zzzzx
                         |'	zzzzx
                         |`	zzzzx
                         |´	zzzzx
                         |&	zzzzc
                         |~	zzzzc
                         |@	zzzzc
                         |#	zzzzc
                         |¶	zzzzc
                         |$	zzzzc
                         |£	zzzzc
                         |€	zzzzc
                         |¥	zzzzc
                         |×	zzzzv
                         |%	zzzzv
                         |‰	zzzzv
                         |½	zzzzv
                         |¼	zzzzv
                         |¾	zzzzv
                         |²	zzzzv
                         |³	zzzzv
                         |()	zzzzb
                         |[]	zzzzb
                         |{}	zzzzb
                         |<>	zzzzb
                         |^	zzzzb
                         |/	zzzzb
                         |\	zzzzb
                         ||	zzzzb
                         |+	zzzzn
                         |-	zzzzn
                         |_	zzzzn
                         |*	zzzzn
                         |¤	zzzzn
                         |?	zzzzn
                         |!	zzzzn
                         |¿	zzzzn
                         |¡	zzzzn""".stripMargin
    val path = Paths.get(pathStr)
    Files.write(path, output.getBytes(StandardCharsets.UTF_8))
  }

}

object GenerateSpecialCharacters {
  val generate = new GenerateSpecialCharacters()
  val twoletterCombi: List[String] = generate.generateLetterCombinations()
}
