package chbrown

import cc.mallet.types.{Instance, InstanceList, IDSorter, Token, TokenSequence}
import cc.mallet.pipe._
import cc.mallet.pipe.iterator.CsvIterator
import cc.mallet.topics.ParallelTopicModel

import scala.collection.JavaConversions._ // implicit

trait InstanceCorpus {
  def instances: Seq[Instance]
}

class MalletTopicModel(N: Int, a: Double = 5.0, b: Double = 0.1) extends ParallelTopicModel(N, a, b) {
  // From https://gist.github.com/1763193
  setTopicDisplay(100, 20)
  setNumIterations(500)
  setOptimizeInterval(25)
  setBurninPeriod(200)
  setSymmetricAlpha(false)
  setNumThreads(Runtime.getRuntime.availableProcessors)

  def topicAssociations(): Seq[List[(String, Int)]] = {
    // code sort of from mith-jvm-lib/mining/src/main/scala/edu/umd/mith/mining/topic/TopicModel.scala
    // get back an (unordered) List of Lists of (token, weight) pairs, sorted descending on the weights
    // topic is a TreeSet[IDSorter], word is a IDSorter
    getSortedWords.map { topic =>
      topic.iterator.map { word =>
        (alphabet.lookupObject(word.getID).toString, word.getWeight.toInt)
      }.toList.sortWith((a, b) => (a._2 > b._2))
    }
  }

  def printTopWords(count:Int = 10) {
    println("Top words")
    // ArrayList<TreeSet<IDSorter>>
    val topicSortedWords = getSortedWords()
    (0 until numTopics).foreach { topic_i =>
      val sortedWords = topicSortedWords.get(topic_i).iterator.map { id_sorter =>
        alphabet.lookupObject(id_sorter.getID)
      }
      println("\n# Topic "+topic_i+": ")
      println(sortedWords.take(count).mkString(" "))
    }
  }

  // def topicRankings(index: Int) = {
    // these are the thetas from the model (I think)
    // returns list of (weight, topic index) pairs, sorted descending on weight:
    // List((0.794, 4), (0.058, 1), etc.
    // getTopicProbabilities(index).zipWithIndex.sortBy(_._1).reverse
  // }
}
