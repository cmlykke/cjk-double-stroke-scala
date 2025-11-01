package AcodeGenerator

import AcodeGenerators.ArollOutConway
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ArollOutConwayTest extends AnyFlatSpec with Matchers {

  it should "test rollout codes" in {

    val test1 = ArollOutConway.roolOutConway("123(34|45)111")
    test1 shouldBe Set("12345111","12334111")

    val test2 = ArollOutConway.roolOutConway("123(|45)111")
    test2 shouldBe Set("12345111", "123111")
  }
}
