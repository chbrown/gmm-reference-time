package chbrown.ohio

import chbrown.{MalletTopicModel, InstanceCorpus, ArgMapper, Tabular, Lexicon}

import cc.mallet.types.{Instance, InstanceList, IDSorter, Token, TokenSequence}
import cc.mallet.pipe._
import scala.collection.JavaConversions._ // implicit

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
