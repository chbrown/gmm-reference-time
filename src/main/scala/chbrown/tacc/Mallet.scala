package chbrown.tacc

import chbrown.{MalletTopicModel, ArgMapper, Tabular, Lexicon}
import chbrown.{RichString, Lowercase, Tokenize, RemoveStopwords, Instantiate, InstanceListify}


import scala.collection.JavaConversions._ // implicit
import scala.collection.JavaConverters._ // asScala
import scala.util.control.ControlThrowable

import cc.mallet.types.{Instance, InstanceList, IDSorter, Token, TokenSequence}
import cc.mallet.pipe._

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

case class YearDocument(year: Int, text: String)

object Mallet {
  implicit def enrichString(x: String) = new RichString(x)

  // def readInstances(pathIn: String) = { //: List[YearInstance]
  // }

  // case class EB1911(pathIn: String) extends InstanceCorpus {
  // val year_texts = all_year_texts.filter(

  val numerals = "1 2 3 4 5 6 7 8 9 0 000".tokenSet
  val roman_numerals = "ii iii iv".tokenSet
  val letters = "a b c d e f g h j k l m n o p q r s t u v w x y z".tokenSet
  // val common = "also all about who have not has th had been be its this or an but are that
  val common = "and in for to was by a of is as from on it at which with that s were are but or".tokenSet
  //   were as which with it from on at for is by was to and in of the "
  val stopwords = numerals ++ roman_numerals ++ letters ++ common

  def meanDistances(doc_thetas: Seq[(YearDocument, Array[Double])]) {
    val distances = List(0, 1, 2, 5, 10, 25, 50)
    distances.foreach { distance =>
      // val year_pairs = .zip(year_instances.indices.drop(distance))
      val comparisons = doc_thetas.combinations(2).filter {
        case doc_theta1 :: doc_theta2 :: _ =>
          (doc_theta1._1.year - doc_theta2._1.year).abs == distance
      }.take(1000).toList
      val js = comparisons.map {
        case doc_theta1 :: doc_theta2 :: _ =>
          Divergence.JS(doc_theta1._2, doc_theta2._2)
      }
      println("Distance: %3d (%3d) = %.5f" format (distance, comparisons.size, Divergence.Mean(js)))
    }
  }

  // object FilterNotDigits extends Function1[String, String] {
  //   def apply(string: String) = string.
  // }
  val scratch = "/scratch/01613/chbrown"

  def main(argstrings: Array[String]) = {
    // run-main chbrown.tacc.Mallet eb-12k-windows.tsv

    val args = ArgMapper(argstrings, Map(
      "pathIn" -> "eb-12k-windows.tsv",
      "reset" -> "FALSE"))

    val documents = Tabular.read("%s/%s" format (scratch, args("pathIn")), headers=Some(List("year", "text")))
      .filter(_("year") != "xxxx")
      .map(docMap => YearDocument(docMap("year").toInt, docMap("text")))
      .filter(doc => 1700 <= doc.year && doc.year <= 1911)
      .toList

    val pipeline = Lowercase andThen Tokenize("\\w+".r) andThen RemoveStopwords(stopwords) andThen
      { _.filterNot(_.matches("\\d+")) }


    val force = args("reset") != "FALSE"
    val texts = documents.map(_.text).map(pipeline)
    val topicModel = MalletTopicModel.fromFile("tmp/topicmodel.mallet", 50, texts, force=force, alpha=1)

    // val instanceList = InstanceListify()
    // topicModel.addInstances(instanceList)
    // topicModel.setNumIterations(500)
    // topicModel.estimate

    val indices = documents.indices
    val documentThetas = indices.map(topicModel.getTopicProbabilities)
    // val en_thetas = indices.map(_+indices.size).map(topicModel.getTopicProbabilities)

    // 10, 1, 5 are the params used in one of the mallet testcases

    val devListPath = "%s/timepaper/gutenberg/dev/devList.txt" format scratch
    val devDocuments = Tabular.read(devListPath, headers=Some(List("title", "year"))).map { docMap =>
      val title = docMap("title")
      val year = docMap("year").substring(0, 4).toInt
      val textPath = "%s/timepaper/gutenberg/dev/unfiltered/%s_clean.txt" format (scratch, title)
      val text = io.Source.fromFile(textPath).getLines.mkString(" ")
      YearDocument(year, text)
      // (year, title, text)
    }.filter(doc => 1700 <= doc.year && doc.year <= 1911)
    //  filter { case (year, title, text) =>
    //   1700 <= doc.year && doc.year <= 1911)
    // }

    val Instantiator = Instantiate(topicModel.getAlphabet())
    val inferencer = topicModel.getInferencer()
    // (devDocuments, devInstances).take(10).foreach { case ((year, title, text), instance) =>
    devDocuments.foreach { case YearDocument(year, text) =>
      val instance = Instantiator(pipeline(text))
      val theta = inferencer.getSampledDistribution(instance, 100, 10, 500)
      val closestYearDoc = (documentThetas, documents).zipped.map { case (docTheta, doc) =>
        (Divergence.JS(theta, docTheta), doc)
      }.sortBy(_._1).head._2
      println("Actual -> %d :: %d <- Nearest" format (year, closestYearDoc.year))
    }

    // meanDistances(instances.zip(thetas))

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
