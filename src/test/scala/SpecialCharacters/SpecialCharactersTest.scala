package SpecialCharacters

import OverlapCalc.OverlapCalculations
import UtilityClasses.{CharSystem, ConwayUnambigous, Grapheme, StaticFileCharInfo}
import org.scalatest.Ignore
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import staticFileGenerators.Conway.GenerateConwayCodes
import staticFileGenerators.SpecialCharacters.{GenerateSpecialCharacters, ReadSpecialCharacters}

import scala.collection.mutable

class SpecialCharactersTest extends AnyFlatSpec with Matchers {

  //ignore
  it should "generate special character files. Should be diabled when not needed" in {
    val pathToWrite: String = "src/main/scala/staticFileGenerators/SpecialCharacters/"
    val generate = new GenerateSpecialCharacters()


    generate.generatePunctuation(pathToWrite + "punctuation.txt")
    generate.generateShapes("zq", pathToWrite + "geometric.txt", List(("U+25A0", "U+25FF")))
    generate.generateShapes("zw", pathToWrite + "radicals.txt", List(("U+2F00", "U+2FDF"), ("U+2E80", "U+2EFF")))
    generate.generateShapes("ze", pathToWrite + "description.txt", List(("U+2FF0", "U+2FFF")))
    generate.generateShapes("zr", pathToWrite + "symbols.txt", List(("U+3000", "U+303F")))
    generate.generateShapes("zt", pathToWrite + "bopomofo.txt", List(("U+3100", "U+312F"),("U+31A0", "U+31BF")))
    generate.generateShapes("zy", pathToWrite + "strokes.txt", List(("U+31C0", "U+31EF")))
    generate.generateShapes("zu", pathToWrite + "enclosed.txt", List(("U+3200", "U+32FF")))
    generate.generateShapes("zi", pathToWrite + "compatibility.txt", List(("U+3300","U+33FF"), ("U+FE30", "U+FE4F")))

    val test = ""
    //generate.generatePunctuation()
  }

  /*
   GenerateSpecialCharacters {

  //covered uncode blocks
  // Geometric Shapes U+25A0..U+25FF
  // CJK Radicals Supplement U+2E80..U+2EFF, U+2F00..U+2FDF
  // Ideographic Description Characters U+2FF0..U+2FFF
  // CJK Symbols and Punctuation U+3000..U+303F
  // Bopomofo U+3100..U+312F, U+31A0..U+31BF
  // CJK Strokes U+31C0..U+31EF
  // Enclosed CJK Letters and Months U+3200..U+32FF
  // CJK Compatibility U+3300..U+33FF, U+FE30..U+FE4F

  def generateShapes(initial: String, outputPath: String, ranges: List[(String, String)]): Unit
  */

  ignore should "check that the special characters are not dublicates" in {
    val punct: Set[Grapheme] = ReadSpecialCharacters.punctiation
    val special: Set[Grapheme] = ReadSpecialCharacters.specialCharacters
    var dublicate: mutable.HashSet[Grapheme] = mutable.HashSet()
    for (grap <- punct) {
      if (special.contains(grap)) {
        dublicate.add(grap)
      }
    }
    dublicate.size shouldBe 0
  }

  ignore should "check that the special characters are not CJK" in {
    val punct: Set[Grapheme] = ReadSpecialCharacters.punctiation
    val special: Set[Grapheme] = ReadSpecialCharacters.specialCharacters
    val conway: Set[Grapheme] = GenerateConwayCodes.conwaySet

    var dublicate: mutable.HashSet[Grapheme] = mutable.HashSet()
    for (grap <- punct) {
      if (conway.contains(grap)) {//.char.head >= 13312) {
        dublicate.add(grap)
      }
    }

    for (grap <- special) {
      if (conway.contains(grap)) {//(grap.char.head >= 13312) {
        dublicate.add(grap)
      }
    }
    dublicate.size shouldBe 3
    dublicate.map(x => x.char).toSet shouldBe Set("，","：","、")
  }

}
