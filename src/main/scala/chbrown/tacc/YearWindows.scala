package chbrown.tacc

import com.nicta.scoobi.Scoobi._
import com.nicta.scoobi.io.sequence.SequenceInput
import edu.umd.cloud9.collection.wikipedia._
import org.apache.hadoop.io.SequenceFile.{Reader => SequenceFileReader}
import org.apache.hadoop.io.{Writable, IntWritable}

object YearWindows extends ScoobiApp {
  def run() {
    // run cluster chbrown.tacc.YearWindows eb-12k.tsv eb-windows-2
    var from = args(0)
    val to = args(1)

    val eb_windows = fromTextFile(from).flatMap { line =>
      val text = line.split("\t") match {
        case Array(slug, title, text) => text
        case _ => "wtf"
      }

      val margin = 50
      // tokens is a List[(characterIndex: Int, token: String)]
      val tokens = "\\S+".r.findAllIn(text).matchData.map(m => (m.start, m.matched)).toList
      val years = "\\b\\d{4}\\b".r.findAllIn(text).matchData.map(m => (m.start, m.matched)).toList

      // return tuples of (year, string window)
      years.map { case (yearCharIndex, year) =>
        // val closestTokenIndex =
        val window = tokens.indexWhere { case (tokenCharIndex, token) =>
          tokenCharIndex >= yearCharIndex
        } match {
          case -1 => tokens.takeRight(margin * 2)
          case i => tokens.slice(i - margin, i) ++ tokens.slice(i + 1, i + margin)
        }
        // _._2 because we only want the token
        (year, window.map(_._2).mkString(" "))
      }
    }

    val combined_windows = eb_windows.groupByKey.combine((a: String, b: String) => a+" "+b)
    val tsv = combined_windows.map { case (k, v) => k+"\t"+v }

    persist(toTextFile(tsv, to, overwrite=true))
  }
}
