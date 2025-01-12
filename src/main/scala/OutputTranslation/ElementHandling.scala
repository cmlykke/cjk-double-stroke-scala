package OutputTranslation

import UtilityClasses.{Cluster, Grapheme}

object ElementHandling {


  def idsStartWithCluster( charorWord: String,
                                   eachCharIdsCluster: Cluster,
                                   elementCluster: Cluster
                                 ): Boolean = {
    if (charorWord == "這" && elementCluster.graphemes.head.char == "言") {
      val test = ""
    }
    if (charorWord == "誘" && elementCluster.graphemes.head.char == "言") {
      val test = ""
    }
    var CharClusterGraphemes: List[Grapheme] =
      eachCharIdsCluster.graphemes
    var elementClusterGraphemes: List[Grapheme] =
      elementCluster.graphemes

    def doesSublistExistWithShapeCondition(charorWord: String,
                                           list1proto: List[Grapheme],
                                           list2: List[Grapheme]
                                          ): Boolean = {
      var list1 = list1proto
      if (list2.isEmpty) {
        return true // An empty list is considered a sublist of any list
      }
      if (list1.isEmpty) {
        return false // A non-empty list2 cannot be a sublist of an empty list1
      }

      //// move around elements - start
      var movearound = false;
      if (list1.size > 2) {
        movearound = true;
      }
      //road character - ⿺
      val roadlist: List[String] = List("辶", "廴", "乙")
      if (list1proto(0).char == "⿺" && roadlist.contains(list1proto(1).char) && charorWord == "這") {
        list1 = (list1proto.head :: list1proto.tail.tail).appended(list1proto.tail.head)
      }
      //tripple horizontal character - ⿲  eks. 彎"
      if (charorWord == "彎") {
        val test = "" // Unused variable, can remove
      }
      list1 = moveTrippleHorizontalToBeginning(charorWord, list1)

      val sublistLength = list2.length
      //val noshapes = list1.filter(_.isShape == false)

      for (i <- list1.indices) {
        // Check if there's enough room for list2 to be a sublist of list1 starting at i
        if (i + sublistLength <= list1.length) {
          val sublistCandidate = list1.slice(i, i + sublistLength)
          if (sublistCandidate.map(x=>x.char).mkString == list2.map(x=>x.char).mkString) {
            // list2 matches this position in list1, now check the preceding elements
            val precedingElements = list1.slice(0, i)
            val allPrecedingShapes = precedingElements.forall(_.isShape)
            if (allPrecedingShapes) {
              return true
            }
          }
        }
      }
      false
    }

    // Use the helper function to check for the sublist condition
    doesSublistExistWithShapeCondition(charorWord, CharClusterGraphemes, elementClusterGraphemes)
  }



  private def moveTrippleHorizontalToBeginning(
                                                charorWord: String,
                                                list1proto: List[Grapheme]
                                              ): List[Grapheme] = {

    // If the condition on charorWord is not needed to alter any logic, you can remove this entirely.
    if (charorWord == "這") {
      val test = "" // Unused variable, can remove
    }
    // Extract sequences of chars
    val initialShapes = list1proto.takeWhile(_.isShape).map(_.char)
    val allStr = list1proto.map(_.char)
    // Check for conditions
    if (initialShapes.contains("⿲") && allStr.contains("言")) {
      val sayIndex = allStr.indexOf("言") // Find the index of "言"
      val moveToIndex = 1                // Desired index to move "言" to
      // Move "言" to the new index
      val elementToMove = list1proto(sayIndex)                              // Extract the element from its old position
      val removedOld = list1proto.patch(sayIndex, Nil, 1)  // Remove it from the old position
      val result = removedOld.patch(moveToIndex, List(elementToMove), 0)   // Insert at the new position
      result // Return the modified list
    } else {
      list1proto // Return the original list if conditions aren't met
    }
  }
  
}
