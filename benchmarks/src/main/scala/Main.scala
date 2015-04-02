import org.scalameter.api._

import ctvector._

object RangeBenchmark extends PerformanceTest.Quickbenchmark {
  val sizes = Gen.range("size")(300000, 1500000, 300000)
  
  val ranges = for {
    size <- sizes
  } yield 0 until size

  performance of "CtVector" in {
    measure method "map" in {
      using(ranges) in {
        r => r.map(_ + 1)
      }
    }
  }
}