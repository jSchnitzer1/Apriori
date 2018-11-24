package apriori

import java.io.File

object Main  {
  def main(args: Array[String]) = {
    val alg = new AprioriAlgorithm(new File("./dataset/test.dat"))
    time {
      alg.runApriori()
    }
    alg.findAssociations
  }

  def time[R](block: => R): R = {
    // http://biercoff.com/easily-measuring-code-execution-time-in-scala/
    val t0 = System.nanoTime()
    val result = block
    val t1 = System.nanoTime()
    val diff = (t1 - t0)
    println(s"\nElapsed time ${diff.toString} ns ≈ ${(diff / 1000000).toString} ms")
    result
  }
}