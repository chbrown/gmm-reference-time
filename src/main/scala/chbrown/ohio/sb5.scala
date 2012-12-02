import chbrown.{MalletTopicModel, InstanceCorpus, ArgMapper, Tabular, Lexicon}

import cc.mallet.types.{Instance, InstanceList, IDSorter, Token, TokenSequence}
import cc.mallet.pipe._
import scala.collection.JavaConversions._ // implicit

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

object Topics {
  def main(argstrings: Array[String]) = {
    val args = ArgMapper(argstrings, Map(
        "pathIn" -> "/Users/chbrown/corpora/ohio/sb5-b.tsv",
        "pathOut" -> "/Users/chbrown/corpora/ohio/sb5-b.out"))

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
  }

}
