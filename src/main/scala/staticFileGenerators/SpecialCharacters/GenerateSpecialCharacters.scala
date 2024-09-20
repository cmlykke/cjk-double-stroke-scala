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
                         |．	.
                         |，	,
                         |,	,
                         |、	,
                         |　	z
                         |　	zx
                         |〃	zx
                         |“”	zx
                         |‘’	zx
                         |（）	zx
                         |？	zx
                         |！	zx
                         |：	zx
                         |；	zx
                         |　	zxa
                         |〃	zxs
                         |“”	zxd
                         |‘’	zxf
                         |（）	zxg
                         |？	zxh
                         |！	zxj
                         |：	zxk
                         |；	zxl
                         |〇	zc
                         |［］	zc
                         |《》	zc
                         |「」	zc
                         |……	zc
                         |·	zc
                         |〻	zc
                         |〃	zc
                         |々	zc
                         |〇	zca
                         |［］	zcs
                         |《》	zcd
                         |「」	zcf
                         |……	zcg
                         |·	zch
                         |〻	zcj
                         |〃	zck
                         |々	zcl
                         |⸺	zv
                         |–	zv
                         |……	zv
                         |⋯	zv
                         |·	zv
                         |‧	zv
                         |〜	zv
                         |～	zv
                         |〰	zv
                         |⸺	zva
                         |–	zvs
                         |……	zvd
                         |⋯	zvf
                         |·	zvg
                         |‧	zvh
                         |〜	zvj
                         |～	zvk
                         |〰	zvl
                         |［］	zb
                         |【】	zb
                         |〖〗	zb
                         |〈〉	zb
                         |《》	zb
                         |「」	zb
                         |『』	zb
                         |〔〕	zb
                         |﹏﹏	zb
                         |［］	zba
                         |【】	zbs
                         |〖〗	zbd
                         |〈〉	zbf
                         |《》	zbg
                         |「」	zbh
                         |『』	zbj
                         |〔〕	zbk
                         |﹏﹏	zbl
                         |〇	zn
                         |◯	zn
                         |○	zn
                         |●	zn
                         |◎	zn
                         |⭕️	zn
                         |✗	zn
                         |×	zn
                         |△	zn
                         |〇	zna
                         |◯	zns
                         |○	znd
                         |●	znf
                         |◎	zng
                         |⭕️	znh
                         |✗	znj
                         |×	znk
                         |△	znl
                         |０	zmaq
                         |１	zmaw
                         |２	zmae
                         |３	zmar
                         |４	zmat
                         |５	zmay
                         |６	zmau
                         |７	zmai
                         |８	zmao
                         |９	zmap
                         |Ａ	zmsa
                         |Ｂ	zmsb
                         |Ｃ	zmsc
                         |Ｄ	zmsd
                         |Ｅ	zmse
                         |Ｆ	zmsf
                         |Ｇ	zmsg
                         |Ｈ	zmsh
                         |Ｉ	zmsi
                         |Ｊ	zmsj
                         |Ｋ	zmsk
                         |Ｌ	zmsl
                         |Ｍ	zmsm
                         |Ｎ	zmsn
                         |Ｏ	zmso
                         |Ｐ	zmsp
                         |Ｑ	zmsq
                         |Ｒ	zmsr
                         |Ｓ	zmss
                         |Ｔ	zmst
                         |Ｕ	zmsu
                         |Ｖ	zmsv
                         |Ｗ	zmsw
                         |Ｘ	zmsx
                         |Ｙ	zmsy
                         |Ｚ	zmsz
                         |ａ	zmda
                         |ｂ	zmdb
                         |ｃ	zmdc
                         |ｄ	zmdd
                         |ｅ	zmde
                         |ｆ	zmdf
                         |ｇ	zmdg
                         |ｈ	zmdh
                         |ｉ	zmdi
                         |ｊ	zmdj
                         |ｋ	zmdk
                         |ｌ	zmdl
                         |ｍ	zmdm
                         |ｎ	zmdn
                         |ｏ	zmdo
                         |ｐ	zmdp
                         |ｑ	zmdq
                         |ｒ	zmdr
                         |ｓ	zmds
                         |ｔ	zmdt
                         |ｕ	zmdu
                         |ｖ	zmdv
                         |ｗ	zmdw
                         |ｘ	zmdx
                         |ｙ	zmdy
                         |ｚ	zmdz
                         |，	zmfa
                         |．	zmfs
                         |：	zmfd
                         |；	zmff
                         |！	zmfg
                         |？	zmfh
                         |＂	zmfj
                         |＇	zmfk
                         |｀	zmfl
                         |＾	zmga
                         |～	zmgs
                         |￣	zmgd
                         |＿	zmgf
                         |＆	zmgg
                         |＠	zmgh
                         |＃	zmgj
                         |％	zmha
                         |＋	zmhs
                         |－	zmhd
                         |＊	zmhf
                         |＝	zmhg
                         |＜	zmhh
                         |＞	zmhj
                         |（）	zmja
                         |［］	zmjs
                         |｛｝	zmjd
                         |｟｠	zmjf
                         |｜	zmjg
                         |￤	zmjh
                         |／	zmjj
                         |＼	zmjk
                         |￢	zmjl
                         |＄	zmka
                         |￡	zmks
                         |￠	zmkd
                         |￦	zmkf
                         |￥	zmkg
                         | 	zzx
                         |.	zzx
                         |,	zzx
                         |:	zzx
                         |;	zzx
                         |"	zzx
                         |'	zzx
                         |`	zzx
                         |´	zzx
                         | 	zzxa
                         |.	zzxs
                         |,	zzxd
                         |:	zzxf
                         |;	zzxg
                         |"	zzxh
                         |'	zzxj
                         |`	zzxk
                         |´	zzxl
                         |&	zzc
                         |~	zzc
                         |@	zzc
                         |#	zzc
                         |¶	zzc
                         |$	zzc
                         |£	zzc
                         |€	zzc
                         |¥	zzc
                         |&	zzca
                         |~	zzcs
                         |@	zzcd
                         |#	zzcf
                         |¶	zzcg
                         |$	zzch
                         |£	zzcj
                         |€	zzck
                         |¥	zzcl
                         |×	zzv
                         |%	zzv
                         |‰	zzv
                         |½	zzv
                         |¼	zzv
                         |¾	zzv
                         |²	zzv
                         |³	zzv
                         |×	zzva
                         |%	zzvs
                         |‰	zzvd
                         |½	zzvf
                         |¼	zzvg
                         |¾	zzvh
                         |²	zzvj
                         |³	zzvk
                         |()	zzb
                         |[]	zzb
                         |{}	zzb
                         |<>	zzb
                         |^	zzb
                         |/	zzb
                         |\	zzb
                         ||	zzb
                         |()	zzba
                         |[]	zzbs
                         |{}	zzbd
                         |<>	zzbf
                         |^	zzbg
                         |/	zzbh
                         |\	zzbj
                         ||	zzbk
                         |+	zzn
                         |-	zzn
                         |_	zzn
                         |*	zzn
                         |¤	zzn
                         |?	zzn
                         |!	zzn
                         |¿	zzn
                         |¡	zzn
                         |+	zzna
                         |-	zzns
                         |_	zznd
                         |*	zznf
                         |¤	zzng
                         |?	zznh
                         |!	zznj
                         |¿	zznk
                         |¡	zznl""".stripMargin
    val path = Paths.get(pathStr)
    Files.write(path, output.getBytes(StandardCharsets.UTF_8))
  }

}

object GenerateSpecialCharacters {
  val generate = new GenerateSpecialCharacters()
  val twoletterCombi: List[String] = generate.generateLetterCombinations()
}
