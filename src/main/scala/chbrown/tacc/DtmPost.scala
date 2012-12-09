package chbrown.tacc

import java.io.File


object DtmPost {
  // run-main chbrown.tacc.DtmPost
  val cd = "/scratch/01613/chbrown/dtm/"
  val lookup = "eb-12k-doc-windows.lookup"

  def recursiveListFiles(f: File): Array[File] = {
    val these = f.listFiles
    these ++ these.filter(_.isDirectory).flatMap(recursiveListFiles)
  }

  def main(args: Array[String]) = {
    val dictionary = io.Source.fromFile(cd+lookup).getLines.map { line =>
      line.split(',') match {
        case Array(index, word) => word
      }
    }.toIndexedSeq

    val children = recursiveListFiles(new File(cd+"eb-1800s-run/lda-seq")).toList
    val years = (1800 until 1900).toList
    // val nameFilter = "topic-\\d+-var-e-log-prob.dat"
    children.filter(_.getName.matches("topic-\\d+-var-e-log-prob.dat")).map { file =>
      println("Reading log prob file: "+file);
      // val term_times has rows that are terms, columns that are times
      val term_times = io.Source.fromFile(file).getLines.map(_.toDouble).grouped(100).toIndexedSeq
      // val term_times has rows that are times, columns that are terms
      // val time_terms = term_times.transpose

      val time_topTokens = years.zipWithIndex.map { case (year, timeIndex) =>
        val logProbs = term_times.map(times => times(timeIndex))
        val topTerms = logProbs.zipWithIndex.sortBy(_._1).take(10).map(_._2)
        (year, topTerms.map(term => dictionary(term)))
      }
      // val time_topTokens = time_terms.zipWithIndex.map { case (terms, time) =>
      //   // terms.length = 142802
      //   val topTen = terms.zipWithIndex.sortBy(_._1).map(_._2).take(10)
      //   val topTenTokens = topTen.map(dictionary)
      //   topTenTokens
      // }

      time_topTokens.foreach { case (year, topTokens) =>
        println("#%d -> %s" format (year, topTokens.mkString(" ")))
      }
    }
  }
}
