package apriori

import java.io._

import scala.collection.mutable
import scala.collection.mutable._
import scala.io.Source
import scala.util.control.Breaks._

class AprioriAlgorithm(inputFile: File) {
  var transactions : ListBuffer[ListBuffer[Int]] = ListBuffer()
  var allItems: ListBuffer[Int] = ListBuffer()
  var candidates: mutable.TreeMap[Int, Int] = mutable.TreeMap()
  var frequentItemsets: mutable.TreeMap[Int, Int] = mutable.TreeMap()
  val minSupport : Double = 1000
  val minConfidence : Double = 0.5

  for (line<-Source.fromFile(inputFile).getLines()) {
    val elementList = mutable.ListBuffer(line.trim.split(' ').map(_.toInt) :_*)
    if (elementList.size > 0) {
      transactions += elementList
    }
  }
  allItems = transactions.flatMap(x => x)

  println(s"No. of items are: ${transactions.size}")
  println(s"No. of baskets are: ${allItems.max + 1}")
  println(s"Support: ${minSupport}")
  println(s"Confidence: ${minConfidence}")

  def runApriori(): Unit = {
    findFrequentSingletons

  }

  private def findFrequentSingletons = {
    candidates = TreeMap(allItems.groupBy(l => l).map(t => (t._1, t._2.length)).toSeq: _*)
    //candidates retain {(key,value) => value > minSupport} // retain only items whos frequent (more than specified support)
    frequentItemsets = candidates.filter(c => c._2 > minSupport)
    //frequentItemsets.foreach(println)
    //candidates.foreach(println)

    println(s"Frequent singletons are: ${frequentItemsets.size}")
    //for ((k,v) <- frequentItemsets) println(s"key: $k, value: $v")

    breakable {
      frequentItemsets.zipWithIndex.foreach { case (kv, i) =>
        print(s"(${kv._1}) ")
        if (i == 25) {
          println("...")
          println(s"There are ${frequentItemsets.size - 25} more frequent singletons")
          break
        }
      }
    }
  }
}
