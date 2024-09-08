package staticFileGenerators.TzaiFrequency
package staticFileGenerators.TzaiFrequency

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class GenerateTzaiMapTest extends AnyFlatSpec with Matchers {

  "The mapTzaiData function" should "correctly map the data from the file" in {
    val mapper = new GenerateTzaiMap()
    val result = mapper.getTzaiMap()

    // Test that the map is not empty
    result should not be empty

    // Assuming the first line of the file is:
    // "1\t的\t7922684\t4.094325317834\tde/di2/di4\t(possessive particle)/of, really and truly, aim/clear"
    // then '的' should map to JundaData(1, 7922684 / mapper.sumThirdColumn())
    val expectedFrequency = 6538132.0 / mapper.getSumThirdColumn()
    result("的") shouldEqual TzaiData(1, expectedFrequency)

    // Add more tests as necessary...
  }
}
