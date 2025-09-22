package Atypes

import Adatasources.FileReaders.AreadConwayData
import staticFileGenerators.Conway.GenerateConwayCodes

import scala.collection.immutable.HashMap
import scala.io.Source

/**
 * Represents a validated Conway notation string.
 * This class ensures the input is a single, non-empty string containing only allowed characters.
 *
 * @param input A list expected to contain exactly one valid Conway string.
 */
case class Aconway(input: List[String]) {
  val rawConway: String = Aconway.verifyInput(input)
}

object Aconway {

  private val allowedChars: Set[Char] = Set('(', ')', '|', '1', '2', '3', '4', '5', '\\')

  /**
   * Verifies the input list and returns the validated string.
   *
   * @param input The input list to verify.
   * @return The validated Conway string.
   * @throws IllegalArgumentException if validation fails.
   */
  def verifyInput(input: List[String]): String = {
    val conwayString = ensureSingleItem(input)
    validateCharacters(conwayString)
  }

  private def validateCharacters(input: String): String = {
    if (input.isEmpty || !input.forall(allowedChars.contains)) {  // Combined checks; used forall for positive intent (all chars allowed).
      throw new IllegalArgumentException(
        s"Input string must be non-empty and contain only allowed characters: ${allowedChars.mkString(", ")} (got: '$input')"
      )
    }
    input
  }

  private def ensureSingleItem(input: List[String]): String = {
    if (input.length != 1) {
      throw new IllegalArgumentException(
        s"Input list must contain exactly one string (got: ${input.length} items: ${input.mkString(", ")})"
      )
    }
    input.head
  }
}