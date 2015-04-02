import org.scalameter.api._

import mbvector._

object MbVectorBenchmark extends PerformanceTest.Quickbenchmark {
  val sizes = Gen.range("size")(300000, 1500000, 300000)
  
  val vectors = for {
    size <- sizes
  } yield new MbVector(size)

  performance of "MbVector" in {
    measure method "map" in {
      using(vectors) in {
        v => v.map(_ + 1)
      }
    }
  }
}