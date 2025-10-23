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
  val rawConway: List[String] = Aconway.verifyInput(input)
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
  def verifyInput(input: List[String]): List[String] = {
    val conwayString = ensureNonZero(input)
    return conwayString
  }


  private def ensureNonZero(input: List[String]): List[String] = {
    if (input.length == 0) {
      throw new IllegalArgumentException(
        s"Input list must contain more than zero (got: ${input.length} items: ${input.mkString(", ")})"
      )
    }
    input
  }
}