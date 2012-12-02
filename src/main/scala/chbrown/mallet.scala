package chbrown

import cc.mallet.types.{Instance, InstanceList, IDSorter, Token, TokenSequence}
import cc.mallet.pipe._
import cc.mallet.pipe.iterator.CsvIterator
import cc.mallet.topics.ParallelTopicModel

import scala.collection.JavaConversions._ // implicit

trait InstanceCorpus {
  def instances: InstanceList
}

case class MalletTopicModel(numTopics:Int, alpha:Double = 5.0, beta:Double = 0.1) {
  val procs = Runtime.getRuntime.availableProcessors
  val model = new ParallelTopicModel(numTopics, alpha, beta)

  // From https://gist.github.com/1763193
  def train(instances:InstanceList, numIterations:Int = 500,
      showTopicInterval:Int = 100, topWordsPerInterval:Int = 20,
      optimizationInterval:Int = 25, burninPeriod:Int = 200,
      useSymmetricAlpha:Boolean = false, numThreads:Int = procs) {
    model.addInstances(instances)
    model.setTopicDisplay(showTopicInterval, topWordsPerInterval)
    model.setNumIterations(numIterations)
    model.setOptimizeInterval(optimizationInterval)
    model.setBurninPeriod(burninPeriod)
    model.setSymmetricAlpha(useSymmetricAlpha)
    model.setNumThreads(numThreads)
    model.estimate
  }

  def topicAssociations(): Seq[List[(String, Int)]] = {
    // code sort of from mith-jvm-lib/mining/src/main/scala/edu/umd/mith/mining/topic/TopicModel.scala
    // get back an (unordered) List of Lists of (token, weight) pairs, sorted descending on the weights
    // topic is a TreeSet[IDSorter], word is a IDSorter
    model.getSortedWords.map { topic =>
      topic.iterator.map { word =>
        (model.alphabet.lookupObject(word.getID).toString, word.getWeight.toInt)
      }.toList.sortWith((a, b) => (a._2 > b._2))
    }
  }

  def printTopWords(count:Int = 10) {
    println("Top words")
    // ArrayList<TreeSet<IDSorter>>
    val topicSortedWords = model.getSortedWords()
    (0 until model.numTopics).foreach { topic_i =>
      val sortedWords = topicSortedWords.get(topic_i).iterator.map { id_sorter =>
        model.alphabet.lookupObject(id_sorter.getID)
      }
      println("\n# Topic "+topic_i+": ")
      println(sortedWords.take(count).mkString(" "))
    }
  }

  def topicRankings(documentIndex: Int) = {
    // these are the thetas from the model (I think)
    // returns list of (weight, topic index) pairs, sorted descending on weight:
    // List((0.794, 4), (0.058, 1), etc.
    model.getTopicProbabilities(documentIndex).zipWithIndex.sortBy(_._1).reverse
  }
}
