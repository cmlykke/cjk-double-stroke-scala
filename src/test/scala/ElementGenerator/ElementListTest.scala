package ElementGenerator

import OverlapCalc.OverlapCalculations
import OverlapCalc.OverlapCalculations.staticfile
import UtilityClasses.InputSizes.Three_oneAndFive_one
import UtilityClasses.{CharSystem, ConwayUnambigous, Grapheme, StaticFileCharInfo, StaticFileCharInfoWithLetterConway}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.mutable

class ElementListTest extends AnyFlatSpec with Matchers {

  def countEntriesInMap(map: mutable.Map[ConwayUnambigous, mutable.Set[StaticFileCharInfo]]): Int = {
    map.values.flatten.size
  }

  it should "check Junda - check decoration with elements" in {

    val ajustClass = ElementAdjustedCodes
    val overlapJUNDA = ElementAdjustedCodes.secondOverlapJunda
    
    val jundaUPDATEDformatted = FormatUtils.formatFinalRes(overlapJUNDA, CharSystem.Junda)
    
    val expected2 = "00 - 25 11 12 34 - 題5105 嗉5351 顆7901\n01 - 12 21 11 34 - 聩5209 綦5599 葜6009 蓁6630 職7201 褧7565\n02 - 12 25 11 34 - 砹5772 硖5856 碶6293 礎6546 礙6767 礞7088 頁7491\n03 - 43 12 34 34 - 糇5898 敉6086 糞6747 粎6785 糷7630 糢7918\n04 - 31 11 54 34 - 锿5941 镓6079 锬6189 镔6399 钬6713 镟6729 镲7557 镦7920\n05 - 31 11 53 54 - 锾6247 铥7445\n06 - 35 25 12 11 - 鲳6372 鮊6549 鲴6565 鯝7144 鲊7344 鯡7717 鳣7781 鮨7927 鱣7928\n07 - 35 25 12 34 - 鳜6451 鱇6484 鲽6555 鲦6606 鳈6667 鮡6844 鰜6922 鯮6926 鳀6931 鱵7119 鰁7145 鱤7146 鱥7295 鯕7522 鲯7528 鲼7959\n08 - 31 11 51 34 - 铗6536 铼7005 锳7957\n09 - 12 15 31 34 - 趱6578 葴7311 趩7351 趧7680 薾7820\n10 - 31 11 53 34 - 铫6706\n11 - 12 21 25 34 - 鞮6826 靹7411 靺7443\n12 - 35 25 12 54 - 鱯7018 鲙7151 鮻7297 鳆7343 鮍7716 鮼7727\n13 - 35 25 12 21 - 鲣7261 鰪7397 鰉7405 鰹7909\n14 - 12 23 52 34 - 薿7294\n15 - 12 23 52 34 - 薿7294\n16 - 12 12 51 34 - 菉7469\n17 - 35 25 12 34 - 鯕7522\n18 - 21 12 25 11 - 蓇7548\n19 - 21 12 25 11 - 蓇7548\n20 - 13 25 13 34 - 磜7711 磫7932\n21 - 12 22 51 34 - 茣7756 菒7757\n22 - 12 22 51 34 - 菒7757\n23 - 12 22 51 34 - 菒7757\n24 - 12 21 25 21 - 雚7773 鞚7789 菫7953\n25 - 31 11 52 34 - 镤7837\n26 - 12 53 51 34 - 醾7956\n27 - 12 53 51 54 - 迺7986"

    val snip: String = FormatUtils.summarizeOverlap(overlapJUNDA)
    
    jundaUPDATEDformatted shouldBe expected2
    snip shouldBe "First 3 keys: [5105, 5209, 5772], Total keys: 28, Total beyond index 8: 97"
    
  }



  it should "check Tzai - check decorations with elements" in {

    val ajustClass = ElementAdjustedCodes
    val overlapTzai = ElementAdjustedCodes.secondOverlapTzai

    val tzaiUPDATEDformatted = FormatUtils.formatFinalRes(overlapTzai, CharSystem.Tzai)

    val expected2 = "00 - o 34 12 34 - 騾5168 驃5281 騄5405 騬6128 騋6637 騃7017 騵7696 騥7948 騠7949\n01 - o 34 12 34 - 騾5168 驃5281 騄5405 騬6128 騋6637 騃7017 騵7696 騥7948 騠7949\n02 - 12 21 25 34 - 鞅5412 顴5454 茦5879 蔌6853\n03 - 12 25 11 34 - 饜5477 戛5769 礞6560 硤7317\n04 - 25 12 51 34 - 嘳5724\n05 - 25 11 12 34 - 暪5811 杲6045 嗉6229 昺6787 曚6866 暵7865\n06 - 44 12 51 34 - 漯5821 灦6175 淏6807 湜6969 澖6992 浿7744\n07 - 35 25 12 34 - 鯕6470 鰷6649 鱌6651 鱳6887 鯀7234 鰬7425 鮢7917\n08 - 35 25 12 34 - 鯕6470 鰷6649 鱌6651 鱳6887 鯀7234 鰬7425 鮢7917\n09 - 12 21 25 34 - 藈6869 虆7042\n10 - 12 21 25 34 - 藈6869 虆7042\n11 - 43 12 34 34 - 糷6888\n12 - 12 21 21 14 - 蘁7032 葙7333\n13 - 12 21 21 14 - 蘁7032 葙7333\n14 - 12 21 11 34 - 聵7181 綦7350 歁7554 聝7571\n15 - 12 12 51 34 - 蘗7235 菉7540 歖7600 蛓7800\n16 - 12 15 31 34 - 葴7334\n17 - 44 12 52 41 - 灗7670\n18 - 25 11 21 34 - 嗏7805\n19 - 12 52 41 34 - 雵7830\n20 - 41 34 11 34 - 贇7942 癵7984\n21 - 35 25 12 11 - 鰆7959\n22 - 35 25 12 11 - 鰆7959"
    
    val snip: String = FormatUtils.summarizeOverlap(overlapTzai)
    
    tzaiUPDATEDformatted shouldBe expected2
    snip shouldBe "First 3 keys: [5168, 5168, 5412], Total keys: 23, Total beyond index 8: 78"
    val test = ""

  }

  
}
