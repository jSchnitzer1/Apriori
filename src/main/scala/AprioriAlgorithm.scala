package apriori

import java.io._
import scala.collection.mutable
import scala.collection.mutable._
import scala.io.Source
import scala.util.control.Breaks._

class AprioriAlgorithm(inputFile: File, minSupportPercentage: Double = 0.5, minConfidence: Double = 0.8) {
  var transactions: ListBuffer[ListBuffer[Int]] = ListBuffer()
  var allItems: ListBuffer[Int] = ListBuffer()
  var candidatesCombinations: ListBuffer[Array[Int]] = ListBuffer()
  var frequentSingletons: ListBuffer[Array[Int]] = ListBuffer()
  var mFrequentSingletons: mutable.Map[Int, Int] = mutable.Map()
  var mFrequentItemset: mutable.Map[Array[Int], Int] = mutable.Map()
  var allFrequentItemsets: ListBuffer[Array[Int]] = ListBuffer()
  var minSupport: Int = 0

  for (line <- Source.fromFile(inputFile).getLines()) {
    if (!line.isEmpty) {
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

  /**
    * To run the algorithm and find all frequent itemset
    * including singletons, doubletons, ...
    */
  def runApriori(): Unit = {
    findFrequentSingletons
    findFrequentItemsets
  }

  /**
    * This method is used to find singletons out of all available
    * items in transactions
    */
  private def findFrequentSingletons: Unit = {
    var candidates: ListBuffer[Array[Int]] = ListBuffer()
    var freq: Array[Int] = Array.fill(allItems.max + 1)(0)
    for (i <- 0 until allItems.max + 1) {
      candidates += Array(i)
    }

    val it = transactions.iterator
    while (it.hasNext)
      for (item <- it.next()) {
        freq(item) += 1
      }

    for (i <- 0 until candidates.size) {
      if (freq(i) >= minSupport) {
        frequentSingletons += candidates(i)
        mFrequentSingletons.put(candidates(i)(0), freq(i))
      }
    }

    println(s"Frequent singletons are: ${frequentSingletons.size}")
    breakable {
      for (i <- 0 until frequentSingletons.size) {
        print(s"(${frequentSingletons(i)(0)}) ")
        if (i == 25) {
          println("...")
          println(s"There are ${frequentSingletons.size - 25} more frequent singletons")
          break
        }
      }
    }
  }

  /**
    * This method is used to find frequent itemsets where k > 1
    */
  private def findFrequentItemsets: Unit = {
    var k = 2
    var freq: ListBuffer[Array[Int]] = frequentSingletons
    allFrequentItemsets ++= freq

    while (k < 4) {
      genetateCandidatesCombinations(k, freq)
      var candidatesFrequency: ListBuffer[Int] = ListBuffer.fill(candidatesCombinations.size)(0)

      val itT = transactions.iterator
      while (itT.hasNext) {
        var t = itT.next()
        val itC = candidatesCombinations.iterator
        var j = 0
        while (itC.hasNext) {
          val candidateItem = itC.next()
          var contained = true
          breakable {
            val itCI = candidateItem.iterator
            while (itCI.hasNext) {
              val ci = itCI.next()
              if (!t.contains(ci)) {
                contained = false
                break
              }
            }
          }
          if (contained) candidatesFrequency(j) += 1
          j += 1
        }
      }

      freq = filterCandidates(candidatesFrequency)
      allFrequentItemsets ++= freq
      printFrequentItemset(k, freq)
      k += 1
    }
  }


  /**
    * This method is used to print itemset
    * @param k the current pass of apriori
    * @param itemSet the itemset to be printed
    */
  private def printFrequentItemset(k: Int, itemSet: ListBuffer[Array[Int]]) = {
    //val itemSet: ListBuffer[ListBuffer[Int]] = freq.asInstanceOf[]]
    val sType = k match {
      case 2 => "doubletons"
      case 3 => "tripletons"
    }
    println(s"\nFrequent ${sType} are: ${itemSet.size}")
    breakable {
      (0 until itemSet.size) foreach { i =>
        print("(")
        (0 until itemSet(i).size) foreach { j =>
          print({
            itemSet(i)(j)
          })
          if (j != itemSet(i).size - 1) print(", ")
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

  /**
    *
    * @param candidatesFrequency
    * @return
    */
  private def filterCandidates(candidatesFrequency: ListBuffer[Int]): ListBuffer[Array[Int]] = {
    var freq: ListBuffer[Array[Int]] = ListBuffer()
    (0 until candidatesFrequency.size) foreach { i =>
      if (candidatesFrequency(i) >= minSupport) {
        freq += candidatesCombinations(i)
        mFrequentItemset.put(candidatesCombinations(i), candidatesFrequency(i))
      }
    }
    freq
  }

  /**
    * This method is used to generate candidates combinations
    * from the frquent itemset that was calculated previously
    * @param k
    * @param itemSet
    */
  private def genetateCandidatesCombinations(k: Int, itemSet: ListBuffer[Array[Int]]): Unit = {
    val result: ListBuffer[Array[Int]] = ListBuffer()
    for (i <- 0 until itemSet.length) {
      val is1 = itemSet(i)
      val size = is1.length

      for (j <- i + 1 until itemSet.length) {
        val is2: Array[Int] = itemSet(j)
        breakable {
          for (a <- 0 until size) {
            if (a != size - 1 && is1(a) != is2(a)) break
            else if (a == size - 1 && is1(a) != is2(a))
              result += is1 :+ is2(a)
          }
        }
      }
    }
    candidatesCombinations = result
  }

  /**
    * this method is used to find all combinations of
    * association rules. for example, if you buy milk, and coke,
    * then you are likely to by bread
    */
  def findAssociations(): Unit = {
    var it = allFrequentItemsets.iterator
    while (it.hasNext) {
      val itemset = it.next()
      if (itemset.length == 2) {
        val (left, right) = itemset.splitAt(1)
        findRuleConfidence(itemset, left, right)
        findRuleConfidence(itemset, right, left)
      } else if(itemset.length > 2) {
        var i = 0
        while (i < itemset.length) {
          val left = itemset(i)
          var right: ListBuffer[Int] = ListBuffer()
          var j = 0
          while (j < itemset.length) {
            if (itemset(j) != left) {
              right += itemset(j)
            }
            j += 1
          }

          findRuleConfidence(itemset, Array(left), right.toArray)
          findRuleConfidence(itemset, right.toArray, Array(left))
          i += 1
        }

      }
    }
  }

  /**
    * this is a help method to find out if the
    * rule is accepted only if it is at least
    * more than or equal the confidence
    * @param itemset item sef
    * @param left the left fraction of the association rule
    * @param right the right fraction of the association rule
    */
  private def findRuleConfidence(itemset: Array[Int], left: Array[Int], right: Array[Int]) = {
    val upFrac: Double = mFrequentItemset(itemset)
    val downFrac: Double = mFrequentSingletons(left(0))
    val conf: Double = (upFrac / downFrac)
    print(s"Rule: ${printArray(left)} => ${printArray(right)} has confidence: ${conf} => ")
    if (conf >= minConfidence) print("accepted\n") else print("not accepted\n")
  }

  /**
    * a helper method that is used to print
    * association rule
    * @param p itemset
    * @return string to print
    */
  private def printArray(p: Array[Int]): String = {
    var result: String = "("
    for (i <- 0 until p.length) {
      if (i + 1 == p.length)
        result += p(i)
      else
        result += p(i) + ", "
    }
    result += ")"
    result
  }
}
