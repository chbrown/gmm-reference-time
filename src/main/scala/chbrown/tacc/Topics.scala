import cc.mallet.types.{Instance, InstanceList, IDSorter, Token, TokenSequence}
import cc.mallet.pipe._
import cc.mallet.pipe.iterator.CsvIterator
import cc.mallet.topics.ParallelTopicModel

import scala.collection.mutable.{ListBuffer,Map => MutableMap}
import scala.collection.JavaConversions._ // implicit
import scala.collection.JavaConverters._ // asScala
import scala.util.control.ControlThrowable

import scala.io.Source._
import java.util.TreeSet
// Lots of help from https://gist.github.com/1763193
// http://mallet.cs.umass.edu/topics.php

object Tsv {
  def read(filepath: String) = {
    // assumes line breaks as record separators and "\t" as field separators. literal \t inside fields is not allowed
    val lines = fromFile(filepath).getLines
    val columns = lines.next.split('\t')
    lines.map { line =>
      val cells = line.split('\t').map { cell =>
        cell.stripPrefix("\"").stripSuffix("\"").replaceAllLiterally("\"\"", "\"")
      }
      columns.zip(cells).toMap
    }
  }
}

object isStopword {
  // from http://www.ranks.nl/resources/stopwords.html
  val tokens = "a about above after again against all am an and any are aren't as at be because been before being below between both but by can't cannot could couldn't did didn't do does doesn't doing don't down during each few for from further had hadn't has hasn't have haven't having he he'd he'll he's her here here's hers herself him himself his how how's i i'd i'll i'm i've if in into is isn't it it's its itself let's me more most mustn't my myself no nor not of off on once only or other ought our ours ourselves out over own same shan't she she'd she'll she's should shouldn't so some such than that that's the their theirs them themselves then there there's these they they'd they'll they're they've this those through to too under until up very was wasn't we we'd we'll we're we've were weren't what what's when when's where where's which while who who's whom why why's with won't would wouldn't you you'd you'll you're you've your yours yourself yourselves".split(' ')

  def apply(token: String) = tokens.contains(token)
}

object Topics {
  val procs = Runtime.getRuntime.availableProcessors

  // From https://gist.github.com/1763193
  def trainModel(instances:InstanceList,
      numTopics:Int,
      alpha:Double = 5.0,
      beta:Double = 0.1,
      numIterations:Int = 500,
      showTopicInterval:Int = 100,
      topWordsPerInterval:Int = 20,
      optimizationInterval:Int = 25,
      burninPeriod:Int = 200,
      useSymmetricAlpha:Boolean = false,
      numThreads:Int = procs) = {
    val topicModel = new ParallelTopicModel(numTopics, alpha, beta)
    topicModel.addInstances(instances)
    topicModel.setTopicDisplay(showTopicInterval, topWordsPerInterval)
    topicModel.setNumIterations(numIterations)
    topicModel.setOptimizeInterval(optimizationInterval)
    topicModel.setBurninPeriod(burninPeriod)
    topicModel.setSymmetricAlpha(useSymmetricAlpha)
    topicModel.setNumThreads(numThreads)
    topicModel.estimate
    topicModel
  }

  def geoTextInstances(pathIn: String) = {
    val stopwordFilter = new TokenSequenceRemoveStopwords()
    stopwordFilter.addStopWords(
      "to m a on in the you to a is it of t that rt u lt s da 1 2 3 4 5".split(" ")
    )
    val pipes = new SerialPipes(List(
      new CharSequence2TokenSequence("\\w+".r.pattern),
      new TokenSequenceLowercase(),
      stopwordFilter,
      new TokenSequence2FeatureSequence()
    ))

    // pathIn lines look like:
    // USER_79321756 2010-03-03T04:55:32 ÃT: 47.528139,-122.197916  47.528139 -122.197916 @USER_77a4822d @USER_2ff4faca okay:) lol. Saying ok to both of yall about to different things!:*
    // USER_79321756 2010-03-03T05:13:34 ÃT: 47.528139,-122.197916  47.528139 -122.197916 RT @USER_5d4d777a: YOURE A FAG FOR GETTING IN THE MIDDLE OF THIS @USER_ab059bdc WHO THE FUCK ARE YOU ? A FUCKING NOBODY !!!!&gt;&gt;Lol! Dayum! Aye!
    val instances = new InstanceList(pipes)
    fromFile(pathIn).getLines.zipWithIndex.foreach { case (line, index) =>
      val parts = line.split("\\t+", 6)
      val clean_text = "http://\\S+".r.replaceAllIn(parts(5), "")
      instances.addThruPipe(new Instance(clean_text, parts(0), index.toString, null))
    }
    instances
  }

  def topicAssociations(model: ParallelTopicModel): Seq[List[(String, Int)]] = {
    // code sort of from mith-jvm-lib/mining/src/main/scala/edu/umd/mith/mining/topic/TopicModel.scala
    // get back an (unordered) List of Lists of (token, weight) pairs, sorted descending on the weights
    // topic is a TreeSet[IDSorter], word is a IDSorter
    model.getSortedWords.map { topic =>
      topic.iterator.map { word =>
        (model.alphabet.lookupObject(word.getID).toString, word.getWeight.toInt)
      }.toList.sortWith((a, b) => (a._2 > b._2))
    }//.toIndexedSeq
  }

  def printTopWords(model:ParallelTopicModel, count:Int = 10) {
      println("Top words")
      // val w = new PrintWriter(outFile)
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

  def main(args: Array[String]) = {
    val in = args(0) // eb-12k-windows.tsv
    val out = args(1)

    val lines =
    val instances = fromFile(in).map { line =>
      val cells = line.split('\t').map { cell =>
        cell.stripPrefix("\"").stripSuffix("\"").replaceAllLiterally("\"\"", "\"")
      }
      columns.zip(cells).toMap
    }


    val topicModel = trainModel(instances, 10, alpha=1, numIterations=500)
    // printTopWords(topicModel, 100)
    // val topicTokenWeights = topicAssociations(topicModel)
    // // convert Map[topic_i -> List[(token, count)]] into
    // // Map[token -> List[(topic_i, weight)]]
    // val tokenLookup = topicTokenWeights.zipWithIndex.flatMap { case (token_weights, topic_index) =>
    //   token_weights.map { case (token, weight) =>
    //     (token, (topic_index, weight))
    //   }
    // }.groupBy(_._1).mapValues(_.map(_._2)).toMap
    // // tokenLookup(token) will return a List of (topic_index, weight) pairs

    // // println(sb5.tweets.map(_.text).mkString(", "))
    // sb5.tweets.groupBy(_.author).foreach { case (author, tweets) =>
    //   // println("\n---" + author + ": " + tweets.map(_.text).mkString(" - "))
    //   // get the list of all (topic_index -> weight) pairs
    //   tweets.foreach { tweet => // flatMap
    //     val topic_weights = tweet.tokens.flatMap(tokenLookup).groupBy(_._1).mapValues(_.map(_._2).sum)
    //     println("@" + tweet.author + ": " + tweet.text)
    //     println("  " + topic_weights.toList.sortBy(_._2).reverse.take(5))
    //   }
    // }

    // Print out the top words, the word by topic probabilities, and
    // document by topic probabilities as dense matrices.
    // val outBase = args(1)
    // printWordSpace(outBase+"-ws.dat", topicModel, numTopics)
    // printDocumentSpace(outBase+"-ds.dat.transpose",
       // topicModel, numDocuments, numTopics)

    // val modeler = new ParallelTopicModel(10)
    println("Done.")
  }
}

// val writer = new java.io.PrintWriter(arguments(1))
// writer.println("}")
// writer.close()
