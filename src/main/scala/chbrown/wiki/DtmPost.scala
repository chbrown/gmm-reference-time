package chbrown.tacc

import chbrown.File

object DtmPost {
  // run-main chbrown.tacc.DtmPost
  val scratch = "/scratch/01613/chbrown"
  val lookup = scratch+"/dtm/eb-12k-doc-windows.lookup"
  // val numTopics = 20
  val numTimes = 100

  def main(args: Array[String]) {
    val dictionary = File.readLines(lookup).map { line =>
      line.split(',') match {
        case Array(index, word) => word
      }
    }.toIndexedSeq

    val children = File.walk(scratch+"/dtm/eb-1800s-20-run/lda-seq").toList
    val years = (1800 until 1900).toList
    val TopicFilenamePattern = "topic-(\\d+)-var-e-log-prob.dat".r
    val lines = children.flatMap { fileName =>
      // .filter(_.getName.matches(topicFilenamePattern))
      fileName.getName match {
        case TopicFilenamePattern(topicIndex) =>
          println("Reading log prob file: "+fileName+" - (Topic# "+topicIndex+")");
          // val term_times has rows that are terms, columns that are times
          val term_times = File.readLines(fileName).map(_.toDouble).grouped(numTimes).toIndexedSeq
          // val term_times has rows that are times, columns that are terms
          // val time_terms = term_times.transpose

          val time_topTokens = years.zipWithIndex.map { case (year, timeIndex) =>
            val logProbs = term_times.map(times => times(timeIndex))
            val topTerms = logProbs.zipWithIndex.sortBy(_._1).take(10).map(_._2)
            (year, topTerms.map(dictionary))
          }

          time_topTokens.map { case (year, topTokens) =>
            "%s\t%d\t%s" format (topicIndex, year, topTokens.mkString(" "))
          }
        case _ =>
          println("Skipping: "+fileName)
          List()
      }
    }
    File.writeLines(scratch+"/dtm/eb-1800s-20-run/lda-seq/top_ten.txt", lines)
  }
}
