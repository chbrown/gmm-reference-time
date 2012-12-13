package chbrown.tacc

import com.nicta.scoobi.Scoobi._

object YearWindows extends ScoobiApp {
  def first_year(slug: String, title: String, text: String) = {
    // return List[(year: Int, document: String)]
    val first_year = "\\b\\d{4}\\b".r.findAllIn(text).find(year => 500 < year.toInt && year.toInt < 2200)
    first_year match {
      case Some(year) => List((year, text))
      case _ => List()
    }
  }
  def windows(slug: String, title: String, text: String, margin: Int = 50) = {
    val margin = 50
    // tokens is a List[(characterIndex: Int, token: String)]
    val tokens = "\\S+".r.findAllIn(text).matchData.map(m => (m.start, m.matched)).toList
    val years = "\\b\\d{4}\\b".r.findAllIn(text).matchData.map(m => (m.start, m.matched)).toList
    // return tuples of (year, string window)
    years.map { case (yearCharIndex, year) =>
      val tokenIndex = tokens.indexWhere(_._1 >= yearCharIndex)
      val window = tokenIndex match {
        case -1 => tokens.takeRight(margin * 2)
        case i => tokens.slice(i - margin, i) ++ tokens.slice(i + 1, i + margin)
      }
      // _._2 because we only want the token
      (year, window.map(_._2).mkString(" "))
    }
  }
  def run() {
    // run chbrown.tacc.YearWindows eb-12k.tsv eb-first-year
    // run chbrown.tacc.YearWindows enwiki-12k.tsv enwiki-first-year
    val Seq(from, to) = args

    // documents is a List[(year: String, document: String)]
    val documents = fromTextFile(from).flatMap { line =>
      line.split("\t") match {
        case Array(slug, title, text) => first_year(slug, title, text)
        case _ => List()
          // val marker = (("xxxx", "No title"), "wtf")
          // List[((String, String), List[String])](marker)
          // List(marker)
      }
    }

    val tsv = documents.groupByKey.combine { (a: String, b: String) =>
      a+" "+b
    } map { case (year, text) =>
      year+"\t"+text
    }
    // val tsv = documents.groupByKey.combine { _+" "+_ } map { _+"\t"+_ }

    persist(toTextFile(tsv, to, overwrite=true))
  }
}
