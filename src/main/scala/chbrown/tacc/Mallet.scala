package chbrown.tacc

import chbrown.{MalletTopicModel, InstanceCorpus, ArgMapper, Tabular, Lexicon}

import scala.collection.JavaConversions._ // implicit
import scala.collection.JavaConverters._ // asScala
import scala.util.control.ControlThrowable

import cc.mallet.types.{Instance, InstanceList, IDSorter, Token, TokenSequence}
import cc.mallet.pipe._

// import scala.io.Source._
// import java.util.TreeSet
// Lots of help from https://gist.github.com/1763193
// http://mallet.cs.umass.edu/topics.php

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

// val instance_list = new InstanceList(new TokenSequence2FeatureSequence())
// io.Source.fromFile(in).getLines.zipWithIndex.foreach { case (line, index) =>
//   // line: "[label-year]\t[all the text]"
//   line.split("\t") match {
//     case Array(year, text) =>
//       val tokens = text.toLowerCase.split("[^'a-z0-9A-Z]+").
//         filterNot(stopwords).
//         filterNot(_.isEmpty)
//       val tokenSequence = new TokenSequence(tokens.map(new Token(_)))
//       val instance = new Instance(tokenSequence, year, index, null)
//       instance_list.addThruPipe(instance)
//   }
// }


// object isStopword {
//   // from http://www.ranks.nl/resources/stopwords.html
//   val tokens = "a about above after again against all am an and any are aren't as at be because been before being below between both but by can't cannot could couldn't did didn't do does doesn't doing don't down during each few for from further had hadn't has hasn't have haven't having he he'd he'll he's her here here's hers herself him himself his how how's i i'd i'll i'm i've if in into is isn't it it's its itself let's me more most mustn't my myself no nor not of off on once only or other ought our ours ourselves out over own same shan't she she'd she'll she's should shouldn't so some such than that that's the their theirs them themselves then there there's these they they'd they'll they're they've this those through to too under until up very was wasn't we we'd we'll we're we've were weren't what what's when when's where where's which while who who's whom why why's with won't would wouldn't you you'd you'll you're you've your yours yourself yourselves".split(' ')

// class TextPipe {
//   def pipe(instance: Instance) = { }
// }

// data, target, name, source
class YearInstance(text: String, val year: Int, name: Object, source: Object) extends Instance(text, year, name, source) {
  def this(text: String, year: Int) = this(text, year, null, null)
}

object Mallet {

  def readInstances(pathIn: String) = { //: List[YearInstance]
    Tabular.read(pathIn, headers=Some(List("Year", "Text"))).map { map =>
      val year = map("Year") match {
        case "xxxx" => 0
        case integer => integer.toInt
      }
      val digitless_text = "\\b\\d+\\b".r.replaceAllIn(map("Text"), "")
      new YearInstance(digitless_text, year)
    } filter { dated_doc =>
      1800 <= dated_doc.year && dated_doc.year <= 1900
    } toList
  }

  // case class EB1911(pathIn: String) extends InstanceCorpus {
  // val year_texts = all_year_texts.filter(

  // val numerals = "1 2 3 4 5 6 7 8 9 0 "
  // val roman_numerals = "ii iii iv "
  // val letters = "a b c d e f g h j k l m n o p q r s t u v w x y z "
  // val common = "also all about who have not has th had been be its this or an but are that
  //   were as which with it from on at for is by was to and in of the "
  // val stopwords = (numerals + roman_numerals + letters + common + "000").split(' ').toSet


  def meanDistances(doc_thetas: Seq[(YearInstance, Array[Double])]) {
    val distances = List(0, 1, 2, 5, 10, 25, 50)
    distances.foreach { distance =>
      // val year_pairs = .zip(year_instances.indices.drop(distance))
      val comparisons = doc_thetas.combinations(2).filter { case List(doc_theta1, doc_theta2) =>
        (doc_theta1._1.year - doc_theta2._1.year).abs == distance
      }.take(1000).toList
      val js = comparisons.map { case List(doc_theta1, doc_theta2) =>
        Divergence.JS(doc_theta1._2, doc_theta2._2)
      }
      println("Distance: %3d (%3d) = %.5f" format (distance, comparisons.size, Divergence.Mean(js)))
    }
  }

  // def doEB1911(args: Map[String, String]) {}

  def main(args: Array[String]) = {
    // run-main chbrown.tacc.Mallet eb-12k-doc-windows.tsv

    // val eb1911 = EB1911("/Users/chbrown/corpora/gmm/eb-12k-windows.tsv")

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

    val cds = "/scratch/01613/chbrown/"
    val Array(inPath) = args
    val eb_instances = readInstances(cds+inPath)
    // val en_instances = readInstances(cds+"enwiki-12k-windows.tsv")
    val instance_list = new InstanceList(pipes)
    instance_list.addThruPipe(eb_instances.toIterator) // ++ en_instances)
    instance_list

    val topicModel = MalletTopicModel(50, a=1)
    topicModel.addInstances(instance_list)
    topicModel.setNumIterations(100)
    topicModel.estimate

    val indices = eb_instances.indices
    val eb_thetas = indices.map(topicModel.getTopicProbabilities)
    // val en_thetas = indices.map(_+indices.size).map(topicModel.getTopicProbabilities)
    meanDistances(eb_instances.zip(eb_thetas))

    // val distances = List(0, 1, 2, 5, 10, 25, 50)
    // distances.foreach { distance =>
    //   val year_pairs = indices.zip(indices.drop(distance))
    //   val js = year_pairs.map { case (year1, year2) =>
    //     val theta1 = eb_thetas(year1)
    //     val theta2 = en_thetas(year2)
    //     Divergence.JS(theta1, theta2)
    //   }
    //   println("Distance: "+distance+" ("+js.length+") = "+Divergence.Mean(js))
    // }

    // eb1911.year_texts.indices.combinations(2).map { case IndexedSeq(i1, i2) =>
    //   val js_divergence = Divergence.JS(thetas(i1), thetas(i2))
    //   (js_divergence, (i1, i2))
    // }.toList.sortBy(_._1).foreach { case (divergence, (i1, i2)) =>
    //   val year1 = eb1911.year_texts(i1).year
    //   val year2 = eb1911.year_texts(i2).year
    //   println(year1+"->"+year2+": "+divergence)
    // }

    // println("model.topicMask: " + model.topicMask)
    // println("model.topicBits: " + model.topicBits)

    // model.alphabet.lookupObject(typeIndex)
  }
  def getTopicTypeCounts(model: MalletTopicModel) = {
    val topicTypeCounts = (0 until model.alphabet.size).flatMap { typeIndex =>
      model.typeTopicCounts(typeIndex).takeWhile(_ > 0).map { topicCount =>
        val topic = topicCount & model.topicMask
        val count = topicCount >> model.topicBits
        (typeIndex, topic, count)
      }
    } groupBy(_._2) mapValues { values =>
      values.map { case (typeIndex, topic, count) =>
        (typeIndex, count)
      }
    }

    topicTypeCounts.foreach { case (topic, typeCounts) =>
      val typeCountsLine = typeCounts.map { case (typeIndex, count) =>
        typeIndex+":"+count
      }.mkString(" ")
      val totalTokens = model.tokensPerTopic(topic)
      println
      println("Topic " + topic + " (" + totalTokens + ") -> " + typeCountsLine.take(500))
    }

    topicTypeCounts
    // val topicCounts =
    // val line = typeIndex + " " + typeString + " " + topicCounts
    // println(line)


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

  }
}