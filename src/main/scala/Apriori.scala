package apriori

import java.io._
import java.util
import java.util.Arrays

import scala.collection.mutable
import scala.collection.mutable._
import scala.io.Source
import scala.util.control.Breaks._

@deprecated
class Apriori(inputFile: File, minSupportPercentage: Double = 0.6, minConfidence: Double = 0.5) {
  var transactions: ListBuffer[ListBuffer[Int]] = ListBuffer()
  var allItems: ListBuffer[Int] = ListBuffer()
  var candidates: mutable.TreeMap[Int, Int] = mutable.TreeMap()
  var candidatesCombinations: ListBuffer[ListBuffer[Int]] = ListBuffer()
  var frequentSingletons: mutable.TreeMap[Int, Int] = mutable.TreeMap()
  var kFrequentItemsets: ListBuffer[Any] = ListBuffer()
  var allFrequentItemsets: ListBuffer[Any] = ListBuffer()
  var minSupport: Int = 0

  for (line <- Source.fromFile(inputFile).getLines()) {
    if(!line.isEmpty) {
      val elementList = mutable.ListBuffer(line.trim.split(' ').map(_.toInt): _*)
      if (elementList.size > 0) {
        transactions += elementList
      }
    }
  }
  minSupport = (transactions.size * minSupportPercentage).toInt
  allItems = transactions.flatMap(x => x)

  println(s"No. of items are: ${allItems.max + 1}")
  println(s"No. of baskets are: ${transactions.size}")
  println(s"Support: ${minSupport}")
  println(s"Confidence: ${minConfidence}")

  def runApriori(): Unit = {
    findFrequentSingletons
    findFrequentItemsets
  }

  private def findFrequentSingletons: Unit = {
    candidates = TreeMap(allItems.groupBy(l => l).map(t => (t._1, t._2.length)).toSeq: _*)
    frequentSingletons = candidates.filter(c => c._2 >= minSupport)
    println(s"Frequent singletons are: ${frequentSingletons.size}")
    breakable {
      frequentSingletons.zipWithIndex.foreach { case (kv, i) =>
        print(s"(${kv._1}) ")
        if (i == 25) {
          println("...")
          println(s"There are ${frequentSingletons.size - 25} more frequent singletons")
          break
        }
      }
    }
  }

  private def findFrequentItemsets: Unit = {
    var k = 2
    var freq: Any = frequentSingletons.keySet.to[ListBuffer]
    allFrequentItemsets += freq

    while (k < 4) {
      genetateCandidatesCombinations(k, freq)
      var candidatesFrequency: ListBuffer[Int] = ListBuffer.fill(candidatesCombinations.size)(0)

      (0 until transactions.size) foreach { i =>
        (0 until candidatesCombinations.size) foreach { j =>
          val candidateItem = candidatesCombinations(j)
          var contained = true
          breakable{
            (0 until candidateItem.size) foreach { x =>
              if (!transactions(i).contains(candidateItem(x))) {
                contained = false
                break
              }
            }
          }
          if(contained) candidatesFrequency(j) += 1
        }
      }

      freq = filterCandidates(candidatesFrequency)
      allFrequentItemsets += freq
      printFrequentItemset(k, freq)
      k += 1
    }
  }

  private def printFrequentItemset(k: Int, freq: Any) = {
    val itemSet: ListBuffer[ListBuffer[Int]] = freq.asInstanceOf[ListBuffer[ListBuffer[Int]]]
    val sType = k match {
      case 2 => "doubletons"
      case 3 => "tripletons"
    }
    println(s"\nFrequent ${sType} are: ${itemSet.size}")
    breakable {
      (0 until itemSet.size) foreach { i =>
        print("(")
        (0 until itemSet(i).size) foreach { j =>
          print({itemSet(i)(j)})
          if(j != itemSet(i).size -1) print(", ")
        }
        print(")")
        if (i == 25) {
          println("...")
          println(s"There are ${itemSet.size - 25} more frequent itemsets")
          break
        }
      }
    }
  }

  private def filterCandidates(candidatesFrequency: ListBuffer[Int]): ListBuffer[ListBuffer[Int]] = {
    var freq: ListBuffer[ListBuffer[Int]] = ListBuffer()
    (0 until candidatesFrequency.size) foreach { i =>
      if(candidatesFrequency(i) >= minSupport) {
        freq += candidatesCombinations(i)
      }
    }
    freq
  }

  private def genetateCandidatesCombinations(k: Int, freq: Any): Unit = {
    if(k == 2) {
      val itemSet: ListBuffer[Int] = freq.asInstanceOf[ListBuffer[Int]]
      candidatesCombinations = itemSet.distinct.combinations(2).map{case Seq(x,y) => ListBuffer(x,y)}.to[ListBuffer]
    } else if (k == 3) {
      val itemSet: ListBuffer[ListBuffer[Int]] = freq.asInstanceOf[ListBuffer[ListBuffer[Int]]]
      candidatesCombinations = itemSet.flatMap(t => t).distinct.combinations(3).map{case Seq(x,y,z) => ListBuffer(x,y,z)}.to[ListBuffer]
    }
  }
}
