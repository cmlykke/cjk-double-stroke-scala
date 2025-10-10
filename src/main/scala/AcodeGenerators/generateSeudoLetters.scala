package AcodeGenerators

import Adatasources.FileReaders.AidsData
import AgraphemeToCodeConverters.AgraphemeToStrokeSet
import Atypes.{AconwayColl, Agrapheme, AidsRecur}

import scala.collection.immutable.HashMap

class generateSeudoLetters {

}

object generateSeudoLetters {
  def getsplitcodesfromchar(graph: Agrapheme, 
                            conwaymap: HashMap[Agrapheme, AconwayColl], 
                            idsmap: HashMap[Agrapheme, String]): Set[List[String]] = {
    val getConwayFromMap: AconwayColl = conwaymap(graph)
    val elemsfound = AidsRecur.findElementmatch(graph, idsmap)
    
    val codeTest1: Set[String] = AgraphemeToStrokeSet.expandAlt(getConwayFromMap.rawConway.rawConway)//.generateStrokeSet(Agrapheme("言"))
    //codeTest1 shouldEqual Set("1111251", "4111251")
    return Set()
  }
}