import org.scalameter.api._

import mbvector._

object MbVectorBenchmark extends PerformanceTest.Quickbenchmark {
  val sizes = Gen.range("size")(300000, 1500000, 300000)

  override def executor = new org.scalameter.execution.LocalExecutor(
    Warmer.Default(),
    Aggregator.average,
    measurer)

  val vectors = for {
    size <- sizes
  } yield new MbVector[Long](size)

  performance of "MbVector" in {
    measure method "map" in {
      using(vectors) setUp {
        v =>
          v.map(_ + 1)
          v.map(_ + 2)
      } in {
        v => v.map(_ + 1)
      }
    }
  }
}