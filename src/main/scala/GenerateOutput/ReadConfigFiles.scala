package GenerateOutput

import UtilityClasses.OutputEntry

import java.nio.file.{Files, Paths}
import scala.collection.immutable.SortedMap
import scala.collection.mutable.ListBuffer
import scala.io.Source
import scala.util.Using

class ReadConfigFiles {


  def readConfig(filename: String): List[String] = {
    // Define the relative path to the config file starting from the src directory
    val relativePath = Paths.get("src", "main", "scala", "staticFileGenerators", "staticFiles", "configFiles", filename)

    // Read file and return lines as List[String]
    if (Files.exists(relativePath) && Files.isRegularFile(relativePath)) {
      Using(Source.fromFile(relativePath.toFile)) { source =>
        source.getLines().toList
      } match {
        case scala.util.Success(lines) => lines
        case scala.util.Failure(exception) =>
          println(s"Error reading file: $exception")
          List.empty[String]
      }
    } else {
      println(s"File does not exist: $relativePath")
      List.empty[String]
    }
  }
}
