package chbrown

import cc.mallet.types.{Instance, InstanceList, Alphabet, IDSorter, Token, TokenSequence, FeatureSequence}
import cc.mallet.pipe._
import cc.mallet.pipe.iterator.CsvIterator
import cc.mallet.topics.ParallelTopicModel

import scala.collection.JavaConversions._ // implicit

// trait InstanceCorpus {
//   def instances: Seq[Instance]
// }

class RichString(str: String) {
  def tokenSet = str.split(' ').toSet
}
object UnencodeHtml extends Function1[String, String] {
  def apply(string: String) = string
    .replaceAllLiterally("&#x2019;", "'")
    .replaceAllLiterally("&#x201c;", "\"")
    .replaceAllLiterally("&#x201d;", "\"")
    .replaceAllLiterally("&amp;", "&")
    .replaceAllLiterally("&quot;", "\"")
    .replaceAllLiterally("&apos;", "'")
}
object RemoveUrls extends Function1[String, String] {
  def apply(string: String) = "http://\\S+".r.replaceAllIn(string.toString, "")
}
object Lowercase extends Function1[String, String] {
  def apply(string: String) = string.toLowerCase
}
case class Tokenize(wordRegex: util.matching.Regex) extends Function1[String, Seq[String]] {
  def apply(string: String) = wordRegex.findAllIn(string).toSeq
}
case class RemoveStopwords(stopwords: Set[String]) extends Function1[Seq[String], Seq[String]] {
  def apply(tokens: Seq[String]) = tokens.filterNot(stopwords)
}
case class Instantiate(alphabet: Alphabet) extends Function1[Seq[String], Instance] {
  def apply(tokens: Seq[String]) = {
    val fs = new FeatureSequence(alphabet, tokens.size);
    tokens.foreach(fs.add)
    new Instance(fs, null, null, null)
  }
}
object InstanceListify {
  def apply(documents: TraversableOnce[Seq[String]]): InstanceList = {
    val alphabet = new Alphabet()
    val instance_list = new InstanceList(new Noop())
    val instantiator = Instantiate(alphabet)
    documents.map(instantiator).foreach(instance_list.add(_))
    instance_list
  }
}

object MalletTopicModel {
  def fromFile(filePath: String,
    numTopics: Int,
    texts: TraversableOnce[Seq[String]],
    force: Boolean = false,
    alpha: Double = 5.0,
    numIterations: Int = 500) = { // : MalletTopicModel | : ParallelTopicModel
    var savedModel = new java.io.File(filePath)
    // val topicModel =
    if (force || !savedModel.exists()) {
      val topicModel = new MalletTopicModel(numTopics, alpha)
      topicModel.setNumIterations(numIterations)

      val instance_list = InstanceListify(texts)
      topicModel.addInstances(instance_list)
      topicModel.estimate

      topicModel.write(savedModel)
      topicModel
    }
    else {
      ParallelTopicModel.read(savedModel)//.asInstance(MalletTopicModel)
    }
  }
}

class MalletTopicModel(numTopics: Int, alpha: Double = 5.0, beta: Double = 0.1) extends ParallelTopicModel(numTopics, alpha, beta) {
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
