package chbrown.ohio

import chbrown.{MalletTopicModel, ArgMapper, Tabular, Lexicon, File}
import chbrown.{UnencodeHtml, RemoveUrls, RichString, Lowercase, Tokenize, RemoveStopwords, InstanceListify}
import collection.mutable.{ListBuffer, Map=>MutableMap}

import cc.mallet.pipe._
import cc.mallet.topics.ParallelTopicModel

import scala.collection.JavaConversions._ // implicit

// import org.apache.commons.logging.LogFactory
// import org.apache.log4j.Logger
// import org.apache.log4j.Level
// import opennlp.scalabha.cluster.{Kmeans, EuclideanDistance, Point}

import breeze.classify._
import breeze.data._
import breeze.linalg._

// class TweetInstance(text: String, author: String, id: String) extends Instance(text) {
  // val tokens = "[a-z]['a-z]*".r.findAllIn(tweet.text.toLowerCase).toList
  // val tokens = clean.split("[^'a-z0-9A-Z]+").filterNot(stopwords).filterNot(_.isEmpty)

case class Tweet(text: String, author: String, id: String)

object TweetCorpus {
  def apply(pathIn: String = "/Users/chbrown/corpora/ohio/sb5-b.tsv") = {
    Tabular.read(pathIn).map { row =>
      val author = row("Author").split(' ').head
      val issue2 = if (row("For") == "1")
        "for"
      else if (row("Against") == "1")
        "against"
      else if (row("Neutral") == "1")
        "neutral"
      else
        "na"
      Tweet(row("Tweet"), author, row("TweetID"))
    }
  }
}

// Alexander Hamilton: 1, 6–9, 11–13, 15–17, 21–36, 59–61, and 65–85 = 51 total
// James Madison: 10, 14, 37–58 and 62–63 = 26 total
// John Jay: 2–5 and 64 = 5 total
// Madison and Hamilton: 18–20 = 3 total

// disputed: 49–58 Madison, 18–20 Collab, 64 John Jay

case class FederalistPaper(id: Int) {
  // text: String, author: String,
  val lines = ListBuffer[String]()
  var author: String = ""
  lazy val text = lines.map(_.trim).filterNot(_.isEmpty).mkString(" ")
  override def toString = {
    "[%d, %s:]\n%s" format (id, author, text)
  }
}
object FederalistCorpus {
  val GUTENBERG_START = "*** START OF THIS PROJECT GUTENBERG EBOOK THE FEDERALIST PAPERS ***"
  val GUTENBERG_END = "*** END OF THIS PROJECT GUTENBERG EBOOK THE FEDERALIST PAPERS ***"
  val FederalistHeader = "FEDERALIST No. (\\d+)".r
  val Author = "(HAMILTON|MADISON|JAY|MADISON, with HAMILTON)".r
  def apply(pathIn: String = "/Users/chbrown/corpora/FederalistPapers.txt") = {
    val papers = ListBuffer(FederalistPaper(0))
    val lines = io.Source.fromFile(pathIn).getLines.toList
      .dropWhile(_ != GUTENBERG_START)
      .takeWhile(_ != GUTENBERG_END)

    lines.foreach {
      case FederalistHeader(number) => papers += FederalistPaper(number.toInt)
      case Author(author) => papers.last.author = author
      case line => papers.last.lines += line
    }

    papers.drop(1) // drop the dummy paper
  }
}

object Topics {
  implicit def enrichString(x: String) = new RichString(x)

  // val scalabhaLogger = Logger.getLogger(Kmeans.getClass)
  // scalabhaLogger.setLevel(Level.INFO)

  // from http://www.ranks.nl/resources/stopwords.html
  val numerals = "1 2 3 4 5 6 7 8 9 0".tokenSet
  val liberal = "a about above after again against all am an and any are aren't as at be because been before being below between both but by can't cannot could couldn't did didn't do does doesn't doing don't down during each few for from further had hadn't has hasn't have haven't having he he'd he'll he's her here here's hers herself him himself his how how's i i'd i'll i'm i've if in into is isn't it it's its itself let's me more most mustn't my myself no nor not of off on once only or other ought our ours ourselves out over own same shan't she she'd she'll she's should shouldn't so some such than that that's the their theirs them themselves then there there's these they they'd they'll they're they've this those through to too under until up very was wasn't we we'd we'll we're we've were weren't what what's when when's where where's which while who who's whom why why's with won't would wouldn't you you'd you'll you're you've your yours yourself yourselves".tokenSet
  val conservative = "the be been have this with and in for to by a of is as from on it at which that s or".tokenSet

  def main(argstrings: Array[String]) = {
    val cd = "/Users/chbrown/corpora/ohio/"
    val args = ArgMapper(argstrings, Map("pathIn" -> (cd+"sb5-b.tsv"), "pathOut" -> (cd+"sb5-b.out"), "reset" -> "FALSE"))

    val all_documents = TweetCorpus().toList
    val author_counts = all_documents.groupBy(_.author).mapValues(_.size)
    val documents = all_documents.filter { document =>
      author_counts(document.author) > 50
    }
    // val documents = FederalistCorpus()
    // documents.foreach { federalistPaper =>
    //   println(federalistPaper.id + ": " + federalistPaper.author)
    // }

    //  was with were are but
    // "and in for to by a of is as from on it at which that s or 1 2 3 4 5 6 7 8 9 0".split(' ')
    val sb5_stopwords = "sb5 issue2 rt ohio weareohio notgvn".tokenSet
    val federalist_stopwords = "will may can upon".tokenSet
    val pipeline = UnencodeHtml andThen RemoveUrls andThen Lowercase andThen
      Tokenize("['a-z0-9]+".r) andThen RemoveStopwords(liberal ++ numerals ++ sb5_stopwords)
    val texts = documents.map(_.text).map(pipeline)

    var saved_model = new java.io.File("tmp/topicmodel.mallet");
    val topicModel = if (args("reset") != "FALSE" || !saved_model.exists()) {
      val topicModel = new MalletTopicModel(10, alpha=0.1)
      topicModel.setNumIterations(500)

      val instance_list = InstanceListify(texts)
      topicModel.addInstances(instance_list)
      topicModel.estimate

      topicModel.write(saved_model)
      topicModel
    }
    else {
      ParallelTopicModel.read(saved_model)
    }

    val inferencer = topicModel.getInferencer()

    val topic_sentiments = (documents, texts, documents.indices).zipped.map { case (document, tokens, index) =>
      // val topic_ranking = theta.zipWithIndex.sortBy(_._1).reverse
      val matches = Lexicon.LiwcLists(tokens)
      val counts = matches.flatten.groupBy(identity).mapValues(_.size)
      val pos = counts.getOrElse("posemo", 0)
      val neg = counts.getOrElse("negemo", 0)

      topicModel.getTopicProbabilities(index).zipWithIndex.map { case (weight, topicIndex) =>
        (weight*pos, weight*neg)
      }
    }.toList
    // topic_sentiments is a List of List[(pos_weight: Double, neg_weight: Double)]'s (one tuple for each document)

    // author_topic_sentiments is a list of counts of sentiment words directed at topics by authors
    val author_topic_sentiments = (documents, topic_sentiments).zipped.groupBy(_._1.author).mapValues { byAuthor =>
      // impost topic_indices on each topic_sentiments list of weight-tuples
      val topic_weights = byAuthor.map(_._2.zipWithIndex).flatten
      topic_weights.groupBy(_._2).mapValues { byTopic =>
        // val ((pos, neg), index) = byTopic.first
        val pos = byTopic.map(_._1._1).sum
        val neg = byTopic.map(_._1._2).sum
        (pos, neg)
      }.toList.sortBy(_._1)
    }.toList.sortBy(_._1)

    val author_sentiments_csv = author_topic_sentiments.map { case (author, topicSentiments) =>
      val sentiments = topicSentiments.map { case (topicIndex, (pos, neg)) =>
        "%.3f,%.3f" format (pos, neg)
      }
      (author +: sentiments).mkString(",")
    }
    val cols = "User" +: (1 to 10).toList.flatMap(i => List("pos" + i, "neg" + i))
    File.writeLines("author_sentiments.csv", cols.mkString(",") +: author_sentiments_csv)

    documents(1000000000)
    // val author_points = author_topic_sentiments.map { case (author, topicSentiments) =>
    //   val coords = topicSentiments.flatMap { case (topicIndex, (pos, neg)) => List(pos, neg) }
    //   Point(coords.toIndexedSeq)
    // } toIndexedSeq

    // val kmeans = new Kmeans(author_points, EuclideanDistance)
    // val (authorDispersion, authorCentroids) = kmeans.run(3, 10)
    // println("Using best Kmeans dispersion: " + authorDispersion)

    // val document_points = (documents, topic_sentiments).zipped.map { case (document, topicSentiments) =>
    //   val coords = topicSentiments.flatMap { case (pos, neg) => List(pos, neg) }
    //   Point(coords.toIndexedSeq)
    // } toIndexedSeq
    // println(document, coords)
    // val point =
    // val theta = topicModel.getTopicProbabilities(index)
    // theta.zipWithIndex.map { case (weight, topicIndex) =>
    // (document, topicIndex, weight*pos, weight*neg)
    // }

    def LiwcCounter(tokens: Seq[String]) = {
      val categories = List("funct", "pronoun", "ppron", "i", "we", "you", "shehe", "they", "ipron", "article", "verb", "auxverb", "past", "present", "future", "adverb", "preps", "conj", "negate", "quant", "number", "swear", "social", "family", "friend", "humans", "affect", "posemo", "negemo", "anx", "anger", "sad", "cogmech", "insight", "cause", "discrep", "tentat", "certain", "inhib", "incl", "excl", "percept", "see", "hear", "feel", "bio", "body", "health", "sexual", "ingest", "relativ", "motion", "space", "time", "work", "achieve", "leisure", "home", "money", "relig", "death", "assent", "nonfl", "filler")
      val counts = Lexicon.LiwcLists(tokens).flatten.groupBy(identity)
        .mapValues(_.size.toDouble)
      Counter(counts)
    }

    def BoschCounter(tokens: Seq[String]) = {
      val boschsmith_set = Set("as", "our", "upon")
      val counts = tokens.groupBy(identity)
        .filterKeys(boschsmith_set)
        .mapValues(_.size.toDouble)
      Counter(counts)
    }

    // val boschsmith_points = texts.map { tokens =>
    //   val counts = tokens.groupBy(identity).mapValues(_.size)
    //   Point(boschsmith_keywords.map(cat => counts.getOrElse(cat, 0).toDouble / tokens.size).toIndexedSeq)
    // } toIndexedSeq

    val trainer = new LogisticClassifier.Trainer[String, Counter[String, Double]]()
    // 49–58 Madison, 18–20 Collab, 64 John Jay
    val disputedIds = (49 until 58) ++ (18 until 20) :+ (64)
    // val trainingIds = (1 until 85) -- disputedIndices
    val trainingData = (documents, texts).zipped.toList.filterNot { case (document, tokens) =>
      disputedIds.contains(document.id)
    } map { case (document, tokens) =>
      Example(document.author, LiwcCounter(tokens))
    }
    val classifier = trainer.train(trainingData)

    (documents, texts).zipped.toList.filter { case (document, tokens) =>
      disputedIds.contains(document.id)
    } foreach { case (document, tokens) =>
      val label = classifier(LiwcCounter(tokens))
      println(document.author + " (" + document.id + ") -> " + label)
    }



    // val document_kmeans = new Kmeans(liwc_points, EuclideanDistance)
    // val (documentDispersion, documentCentroids) = document_kmeans.run(3)
    // println("Document dispersion")
    // val (dispersion, memberships) = document_kmeans.computeClusterMemberships(documentCentroids)
    // println("Inference dispersion: " + dispersion)
    // (documents, memberships).zipped foreach { case (document, membership) =>
    //   println("Doc %d [%s]: %d" format (document.id, document.author, membership))
    // }

      // println("@%s: %s" format (document.author, tokens.mkString(" "))) // document.text
      // print out top five topic alignments
      // println("  " + topic_ranking.take(5).map { case (weight, i) =>
      // "%d => %.3f" format (i, weight)
      // }.mkString(" "))

      // val posemo_indices = matches.zipWithIndex.filter(_._1.contains("posemo")).map(_._2)
      // val posemo_words = posemo_indices.map(tokens).mkString(" ")
      // val negemo_indices = matches.zipWithIndex.filter(_._1.contains("negemo")).map(_._2)
      // val negemo_words = negemo_indices.map(tokens).mkString(" ")

      // println("  %d+ (%s), %d- (%s)" format (
      //   counts.getOrElse("posemo", 0),
      //   posemo_words,
      //   counts.getOrElse("negemo", 0),
      //   negemo_words))
      // (tweet, tokens)

    // points.foreach { point =>
      // bestCentroids.zipWithIndices.map {
      // EuclideanDistance(point, bestCentroids
    // }

    // val list = List(9, 5, 6, 1)
    // val map = Map(8 -> 5, 4 -> 1, 5 -> 1)
    // map.toList.sortBy(_._2)
    // map mapValues { _ + 100 } toList // is fine
    // map mapValues { _ + 100 } sortBy(_._2) // is fine
    // map.mapValues { _ + 100 }.toList.sortBy(_._2) // is fine
    // map mapValues { _ + 100 } toList sortBy(_._2) // not okay
    // map.toList.sortBy(_._2).toMap.mapValues { _ + 100 } // is fine
    // map.toList.sortBy(_._2) toMap mapValues { _ + 100 } // not okay

    // println("\n\n-- topicTokenWeights --\n\n")
    // val topicTokenWeights = topicModel.topicAssociations()
    // convert Map[topic_i -> List[(token, count)]] into
    // Map[token -> List[(topic_i, weight)]]
    // val tokenLookup = topicTokenWeights.zipWithIndex.flatMap { case (token_weights, topic_index) =>
    //   token_weights.map { case (token, weight) =>
    //     (token, (topic_index, weight))
    //   }
    // }.groupBy(_._1).mapValues(_.map(_._2)).toMap
    // tokenLookup(token) will return a List of (topic_index, weight) pairs

    // ## Print something like:
    //     @RepealSB5Ohio: Even if we lose, #NOon2 campaign has been successful in uniting
    //       ALL wings of the Democratic Party! Feminists, Socialists, Unionists! #Issue2
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
  }

}


    // content.fold
    // val empty = List(List[String]())
    // val l = List(1, 5, 100, 7, 8, 101, 9, 102, 1, 4, 5, 6, 103, 4)
    // //
    // (empty /: l) { (accumulator, item) =>
    //   if (item > 100)
    //     accumulator :+ List(item)
    //   else
    //     accumulator.tail :+ item
    // }
    // val

