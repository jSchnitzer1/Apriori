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
  val minSupport : Double = 1000
  val minConfidence : Double = 0.5

  for (line<-Source.fromFile(inputFile).getLines()) {
    val elementList = mutable.ListBuffer(line.trim.split(' ').map(_.toInt) :_*)
    if (elementList.size > 0) {
      transactions += elementList
    }
  }

  allItems = transactions.flatMap(x => x)
  candidates = TreeMap(allItems.groupBy(l => l).map(t => (t._1, t._2.length)).toSeq:_*)
  candidates.foreach(println)

  println(s"No. of items are: ${transactions.size}")
  println(s"No. of baskets are: ${allItems.max + 1}")
  println(s"Support: ${minSupport}")
  println(s"Confidence: ${minConfidence}")


}
