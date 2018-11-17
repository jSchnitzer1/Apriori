package apriori

import java.io._
import java.util
import java.util.Arrays

import scala.collection.mutable
import scala.collection.mutable._
import scala.io.Source
import scala.util.control.Breaks._

class AprioriAlgorithm(inputFile: File, minSupportPercentage: Double = 0.04, minConfidence : Double = 0.5) {
  var transactions : ListBuffer[ListBuffer[Int]] = ListBuffer()
  var allItems: ListBuffer[Int] = ListBuffer()
  var candidates: mutable.TreeMap[Int, Int] = mutable.TreeMap()
  var candidatesCombinations: List[(Int, Int)] = List()
  var frequentItemsets: mutable.TreeMap[Int, Int] = mutable.TreeMap()
  var minSupport: Double = 0

  for (line<-Source.fromFile(inputFile).getLines()) {
    val elementList = mutable.ListBuffer(line.trim.split(' ').map(_.toInt) :_*)
    if (elementList.size > 0) {
      transactions += elementList
    }
  }
  minSupport = transactions.size * minSupportPercentage
  allItems = transactions.flatMap(x => x)

  println(s"No. of items are: ${transactions.size}")
  println(s"No. of baskets are: ${allItems.max + 1}")
  println(s"Support: ${minSupport}")
  println(s"Confidence: ${minConfidence}")

  def runApriori(): Unit = {
    findFrequentSingletons
    findFrequentItemsets
  }

  private def findFrequentSingletons: Unit = {
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

  private def findFrequentItemsets: Unit = {
    genetateCandidatesCombinations
    var candidatesFrequency: ListBuffer[Int] = ListBuffer.fill(candidatesCombinations.size)(0)
    (0 until transactions.size) foreach { i =>
      (0 until candidatesCombinations.size) foreach { j =>
        val candidateItem = candidatesCombinations(j)
        if(transactions(i).contains(candidateItem._1) && transactions(i).contains(candidateItem._2))
          candidatesFrequency(j) += 1
      }
    }
  }

  private def genetateCandidatesCombinations: Unit = {
    val itemSet = frequentItemsets.keySet.toList
    candidatesCombinations = itemSet.zipWithIndex.flatMap{case (n,x) => itemSet.drop(x+1).map(n -> _)}
    /*var tmpRes: ListBuffer[List[(Int, Int)]] = ListBuffer()
    (0 until itemSet.size) foreach { i =>
      val left = List(itemSet(i))
      val (_, right) = itemSet.splitAt(i+1)
      val res:List[(Int, Int)] = for (left_ <- left; right_ <- right) yield (left_, right_)
      tmpRes += res
    }
    var combinations: ListBuffer[(Int, Int)] = tmpRes.flatMap(x => x)*/
    //combinations.foreach(println)
  }
}
