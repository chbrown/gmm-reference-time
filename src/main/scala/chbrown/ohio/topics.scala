package chbrown.ohio

import cc.mallet.types.{Instance, InstanceList, IDSorter, Token, TokenSequence}
import cc.mallet.pipe._
import cc.mallet.pipe.iterator.CsvIterator
import cc.mallet.topics.ParallelTopicModel

import scala.collection.mutable.{ListBuffer,Map => MutableMap}
import scala.collection.JavaConversions._ // implicit
import scala.collection.JavaConverters._ // asScala
import scala.util.control.ControlThrowable
// import org.clapper.argot._
// import ArgotConverters._

import java.util.TreeSet
// Lots of help from https://gist.github.com/1763193
// http://mallet.cs.umass.edu/topics.php

// class MathList[T: Numeric[T]](xs: List[T]) {
  // def mean[T <% Numeric](values: Seq[T]) = values.foldLeft(0d)(_ + _) / values.size
  // def mean() = xs.sum / xs.size
// }

// object Implicits {
  // implicit def listToMathList[T](xs: List[T]) = new MathList(xs)
  // implicit def listToMathList[T: Numeric](xs: List[T]) = new MathList(xs)
// }

object ArgMapper {
  def apply(args: Seq[String], defaults: Map[String, String] = Map()) = {
    val flag = "-+(\\w+)".r
    val pairs = args.zip(args.drop(1) :+ "$$$")
    defaults ++ pairs.flatMap {
      case (flag(a), flag(b)) => List((a, "true"))
      case (flag(a), value) => List((a, value))
      case (a, "$$$") => List()
      case (a, b) => List(("wtf", a + b))
    }.toMap
  }
}

object Divergence {
  // Info-theory metrics for discrete distributions
  def Mean(xs: Seq[Double]): Double = {
    xs.sum / xs.size
  }
  def PairwiseMean(ps: Seq[Double], qs: Seq[Double]): Seq[Double] = {
    // returns list of numbers
    ps.zip(qs).map { case (p: Double, q: Double) => (p + q) / 2 }
  }
  def KL(ps: Seq[Double], qs: Seq[Double]): Double = {
    ps.zip(qs).map { case (p, q) => p * math.log(p / q) }.sum
  }
  def JS(ps: Seq[Double], qs: Seq[Double]): Double = {
    val ms = PairwiseMean(ps, qs)
    KL(ps, ms)/2 + KL(qs, ms)/2
  }
}


object Lexicon {
  lazy val liwcTrie = {
    ScalaJson.fromFile[Map[String, Any]]("/usr/local/data/liwc_2007.trie")
  }

  def walk(token: String, index: Int, cursor: Map[String, Any]): List[String] = {
    if (cursor.contains("*")) {
      // assert cursor("*") = List[String]
      return cursor("*").asInstanceOf[List[String]]
    }
    else if (cursor.contains("$") && index == token.size) {
      return cursor("$").asInstanceOf[List[String]]
    }
    else if (index < token.size) {
      var letter = token(index).toString
      if (cursor.contains(letter)) {
        val nextCursor = cursor(letter).asInstanceOf[Map[String, Any]]
        return walk(token, index + 1, nextCursor)
      }
    }
    return List()
  }

  def Liwc(tokens: Seq[String]): Map[String, Int] = {
    // document is a tokenized, lower-cased document
    val categories = tokens.map(walk(_, 0, liwcTrie))
    Map("Dic" -> categories.count(_.size > 0), "WC" -> tokens.size) ++
      categories.flatten.groupBy(identity).mapValues(_.size)
  }

  def LiwcLists(tokens: Seq[String]): Seq[List[String]] = {
    // document is a tokenized, lower-cased document
    tokens.map(walk(_, 0, liwcTrie))
  }
}

object Tabular {
  def read(filepath: String, headers: Option[List[String]] = None, field_sep: Char = '\t') = {
    // read from filepath and return a list of maps.
    // literal \t or , inside fields is not allowed at the moment
    val rows = io.Source.fromFile(filepath).getLines.map(
      _.split(field_sep).map(
        _.stripPrefix("\"").stripSuffix("\"").replaceAllLiterally("\"\"", "\"")
      )
    )
    val final_headers = headers match {
      case Some(some_headers) => some_headers
      case None => rows.next.toList
    }
    rows.map(cells => final_headers.zip(cells).toMap)
  }
}

case class Tweet(text: String, author: String, id: String, index: Int) {
  // from http://www.ranks.nl/resources/stopwords.html
  val stopwords = ("a about above after again against all am an and any are aren't as at be because been before being below between both but by can't cannot could couldn't did didn't do does doesn't doing don't down during each few for from further had hadn't has hasn't have haven't having he he'd he'll he's her here here's hers herself him himself his how how's i i'd i'll i'm i've if in into is isn't it it's its itself let's me more most mustn't my myself no nor not of off on once only or other ought our ours ourselves out over own same shan't she she'd she'll she's should shouldn't so some such than that that's the their theirs them themselves then there there's these they they'd they'll they're they've this those through to too under until up very was wasn't we we'd we'll we're we've were weren't what what's when when's where where's which while who who's whom why why's with won't would wouldn't you you'd you'll you're you've your yours yourself yourselves".split(' ') ++ "sb5 issue2 rt 1 2 3 4 5 6 7 8 9 0".split(' ')).toSet

  def unencodeHtml(html: String): String = {
    html.replaceAllLiterally("&#x2019;", "'")
        .replaceAllLiterally("&#x201c;", "\"")
        .replaceAllLiterally("&#x201d;", "\"")
        .replaceAllLiterally("&amp;", "&")
        .replaceAllLiterally("&quot;", "\"")
        .replaceAllLiterally("&apos;", "'")
  }
  def removeUrls(text: String): String = {
    "http://\\S+".r.replaceAllIn(text, "")
  }
  // lowercase, split, remove urls
  val clean = removeUrls(unencodeHtml(text.toLowerCase))
  // val tokens = "[a-z]['a-z]*".r.findAllIn(tweet.text.toLowerCase).toList
  val tokens = clean.split("[^'a-z0-9A-Z]+").filterNot(stopwords).filterNot(_.isEmpty)
  def instance = {
    val tokenSequence = new TokenSequence(tokens.map(new Token(_)))
    new Instance(tokenSequence, author, id, null)
  }
}

trait InstanceCorpus {
  def instances: InstanceList
}

class GeoText(pathIn: String) extends InstanceCorpus {
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
  val instance_list = new InstanceList(pipes)
  io.Source.fromFile(pathIn).getLines.zipWithIndex.foreach { case (line, index) =>
    val parts = line.split("\\t+", 6)
    val clean_text = "http://\\S+".r.replaceAllIn(parts(5), "")
    instances.addThruPipe(new Instance(clean_text, parts(0), index.toString, null))
  }
  def instances = instance_list
}


class SB5(pathIn: String, first: Int = 1000000) extends InstanceCorpus {
  // val lineRegex = "(\\S+)[\\s]+(\\S+)[\\s]+(.+)".r
  val tweets = Tabular.read(pathIn).drop(10000).take(first).zipWithIndex.map { case (row, index) =>
    val author = row("Author").split(' ').head
    new Tweet(row("Tweet"), author, row("TweetID"), index)
  }.toList
  // groupBy returns a list of author->List(tweet_tuples) pairs
  // authors = List((String, List((String, String))))

  def instances = {
    val instance_list = new InstanceList(new TokenSequence2FeatureSequence())
    instance_list.addThruPipe(tweets.toIterator.map(_.instance))
    instance_list
  }
}

case class EB1911(pathIn: String = "/Users/chbrown/corpora/gmm/eb-12k-windows.tsv") extends InstanceCorpus {
  val year_texts = io.Source.fromFile(pathIn).getLines.map { line =>
    line.split('\t') match {
      case Array(year, text) => (year.toInt, text)
    }
  }.filter { case (year, text) =>
    1800 <= year && year <= 1900
  }.toList

  def instances = {
    val stopwordFilter = new TokenSequenceRemoveStopwords()
    stopwordFilter.addStopWords(
      "and in for to was by a of is as from on it at which with that s were are but or 1 2 3 4 5 6 7 8 9 0".split(' ')
    )
    val pipes = new SerialPipes(List(
      new CharSequence2TokenSequence("\\w+".r.pattern),
      new TokenSequenceLowercase(),
      stopwordFilter,
      new TokenSequence2FeatureSequence()
    ))

    val instance_list = new InstanceList(pipes)
    year_texts.zipWithIndex.foreach { case ((year, text), index) =>
      val digitless_text = "\\b\\d+\\b".r.replaceAllIn(text, "")
      instance_list.addThruPipe(new Instance(digitless_text, year, index, null))
    }
    instance_list
  }
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

object Topics {
  def main(argstrings: Array[String]) = {
    // val instances = geoTextInstances(pathIn)
    val args = ArgMapper(argstrings, Map(
        "pathIn" -> "/Users/chbrown/corpora/ohio/sb5-b.tsv",
        "pathOut" -> "/Users/chbrown/corpora/ohio/sb5-b.out"))

    val topicModel = MalletTopicModel(50, alpha=1)

    val eb1911 = EB1911()
    topicModel.train(eb1911.instances, numIterations=500)
    val thetas = eb1911.year_texts.indices.map(topicModel.model.getTopicProbabilities)

    eb1911.year_texts.indices.combinations(2).map { case Vector(i1, i2) =>
      val js_divergence = Divergence.JS(thetas(i1), thetas(i2))
      (js_divergence, (i1, i2))
    }.toList.sortBy(_._1).foreach { case (divergence, (i1, i2)) =>
      val year1 = eb1911.year_texts(i1)._1
      val year2 = eb1911.year_texts(i2)._1
      println(year1+"->"+year2+": "+divergence)
    }

  }

  def meanDistances(thetas: Seq[Seq[Double]], eb1911: EB1911) {
    val distances = List(1, 5, 10, 25, 50)
    distances.foreach { distance =>
      val year_pairs = eb1911.year_texts.indices.zip(eb1911.year_texts.indices.drop(distance))
      val js = year_pairs.map { case (year1, year2) =>
        val theta1 = thetas(year1)
        val theta2 = thetas(year2)
        Divergence.JS(theta1, theta2)
      }
      println("Distance: "+distance+" ("+js.length+") = "+Divergence.Mean(js))
    }
    // val year_pairs = eb1911.year_texts.indices.zip(eb1911.year_texts.indices.drop(1)).map { case (year1, year2) =>

    // }
  }
  def doSB5(args: Map[String, String]) {
    val sb5 = new SB5(args("pathIn"), 1000)
    val topicModel = MalletTopicModel(10, alpha=1)
    topicModel.train(sb5.instances)

    val thetas = sb5.tweets.indices.map(topicModel.model.getTopicProbabilities)

    sb5.tweets.foreach { tweet =>
      val topic_ranking = topicModel.topicRankings(tweet.index)
      println("@" + tweet.author + ": " + tweet.tokens.mkString(" "))
      println("  " + topic_ranking.take(5).map { case (weight, index) =>
        "%d => %.3f".format(index, weight)
      }.mkString(" "))

      val tokens = tweet.tokens
      val counts = Lexicon.Liwc(tokens)
      println("  " + counts.getOrElse("posemo", 0) + "+, " + counts.getOrElse("negemo", 0) + "-")

      val liwc_lists = Lexicon.LiwcLists(tokens)
      val posemo_indices = liwc_lists.zipWithIndex.filter(_._1.contains("posemo")).map(_._2)
      val negemo_indices = liwc_lists.zipWithIndex.filter(_._1.contains("negemo")).map(_._2)
      println("  " + posemo_indices.map(tokens).mkString(" ") + "+, " + negemo_indices.map(tokens).mkString(" ") + "-")
    }
    println("\n\n-- topicTokenWeights --\n\n")
    val topicTokenWeights = topicModel.topicAssociations()
    // convert Map[topic_i -> List[(token, count)]] into
    // Map[token -> List[(topic_i, weight)]]
    val tokenLookup = topicTokenWeights.zipWithIndex.flatMap { case (token_weights, topic_index) =>
      token_weights.map { case (token, weight) =>
        (token, (topic_index, weight))
      }
    }.groupBy(_._1).mapValues(_.map(_._2)).toMap
    // tokenLookup(token) will return a List of (topic_index, weight) pairs

    // ## Print something like:
    //     @RepealSB5Ohio: Even if we lose, #NOon2 campaign has been successful in uniting ALL wings of the Democratic Party! Feminists, Socialists, Unionists! #Issue2
    //       List((2,63), (4,47), (8,33), (5,25), (7,21))
    //     @RepealSB5Ohio: It's just about money! http://t.co/KKoOVcRz #Issue2
    //       List((7,43), (9,15), (5,5), (3,3), (4,2))
    // println(sb5.tweets.map(_.text).mkString(", "))
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
