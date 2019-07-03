import org.scalatest.{FunSuite, Matchers}

class RNGTest extends FunSuite with Matchers {

  test("Generate random numbers within a range correctly") {
    val seeds = List(0L, 42L, 1L, 999L, 81038104L)
    val maxValue = 42

    seeds.foreach { seed =>
      var rng: RNG = SimpleRNG(seed)
      var randomValues = Set.empty[Int]
      for (_ <- 0 to 5) {
        val (randomValue, newRng) = RNG.nonNegativeLessThan(maxValue)(rng)
        rng = newRng
        randomValues = randomValues + randomValue
      }

      randomValues.size should be > 1
      randomValues.foreach { value =>
        value should be >= 0
        value should be < maxValue
      }
    }
  }
}
