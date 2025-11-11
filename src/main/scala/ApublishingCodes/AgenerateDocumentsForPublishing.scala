package ApublishingCodes

import Adatasources.FileReaders.AreadSpecialSymbols
import Asingletons.AsingletonsForTests
import Atypes.AsortingObject

object AgenerateDocumentsForPublishing {

  private val punctuation: List[String] = AreadSpecialSymbols.getLinesFromPunctuation()
  private val outputSImp: List[(String, String, AsortingObject)] = AsingletonsForTests.outputSortedSimp
  private val outputTrad: List[(String, String, AsortingObject)] = AsingletonsForTests.outputSortedTrad
  
  
  def generateOutputSimplifiedString(): List[String] = {
   val onlyStringsList: List[String] = outputSImp.map(x => x._1 +"\t"+x._2)
   val output =  punctuation ++ onlyStringsList
   return output
  }

  def generateOutputTraditionalString(): List[String] = {
    val onlyStringsList: List[String] = outputTrad.map(x => x._1 + "\t" + x._2)
    val output = punctuation ++ onlyStringsList
    return output
  }

  
}
