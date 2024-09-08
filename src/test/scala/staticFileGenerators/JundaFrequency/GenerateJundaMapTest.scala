package staticFileGenerators.JundaFrequency
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import staticFileGenerators.JundaFrequency.{GenerateJundaMap, JundaData}

class GenerateJundaMapTest extends AnyFlatSpec with Matchers {

  "The mapJundaData function" should "correctly map the data from the file" in {
    val mapper = new GenerateJundaMap()
    val result = mapper.getJundaMap()

    // Test that the map is not empty
    result should not be empty

    // Assuming the first line of the file is:
    // "1\t的\t7922684\t4.094325317834\tde/di2/di4\t(possessive particle)/of, really and truly, aim/clear"
    // then '的' should map to JundaData(1, 7922684 / mapper.sumThirdColumn())
    val expectedFrequency = 7922684.0 / mapper.getSumThirdColumn()
    result("的") shouldEqual JundaData(1, expectedFrequency)

    // Add more tests as necessary...
  }
}