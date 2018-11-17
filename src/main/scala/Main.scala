package apriori

import java.io.File

object Main  {
  def main(args: Array[String]) = {
    val alg = new AprioriAlgorithm(new File("./dataset/T10I4D100K.dat"))
    alg.runApriori()
//    println("===Support Items===")
//    alg.toRetItems.foreach(println)
//    println("===Association Rules===")
//    alg.associationRules.foreach(println)
  }
}