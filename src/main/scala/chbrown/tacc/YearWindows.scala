package chbrown.tacc

import com.nicta.scoobi.Scoobi._

object YearWindows extends ScoobiApp {
  def windows() {
    val eb_windows = fromTextFile(from).flatMap { line =>
      line.split("\t") match {
        case Array(slug, title, text) =>
          val margin = 50
          // tokens is a List[(characterIndex: Int, token: String)]
          val tokens = "\\S+".r.findAllIn(text).matchData.map(m => (m.start, m.matched)).toList
          val years = "\\b\\d{4}\\b".r.findAllIn(text).matchData.map(m => (m.start, m.matched)).toList

          // return tuples of (year, string window)
          years.map { case (yearCharIndex, year) =>
            val window = tokens.indexWhere { case (tokenCharIndex, token) =>
              tokenCharIndex >= yearCharIndex
            } match {
              case -1 => tokens.takeRight(margin * 2)
              case i => tokens.slice(i - margin, i) ++ tokens.slice(i + 1, i + margin)
            }
            // _._2 because we only want the token
            ((year, title), window.map(_._2).mkString(" "))
          }
        case _ =>
          // val marker = (("xxxx", "No title"), "wtf")
          // List[((String, String), List[String])](marker)
          // List(marker)
          List()
      }

    }

  }
  def run() {
    // run cluster chbrown.tacc.YearWindows eb-12k.tsv eb-windows-2
    var from = args(0)
    val to = args(1)

    // documents is a List[(year: String, document: String)]
    val documents = fromTextFile(from).flatMap { line =>
      line.split("\t") match {
        case Array(slug, title, text) =>
          "\\b\\d{4}\\b".r.findAllIn(text).find(year => 500 < year.toInt && year.toInt < 2200).match {
            case Some(year) => List((year, text))
            case _ => List()
          }
        case _ => List()
      }

    }

    val tsv = documents.groupByKey.combine { (a: String, b: String) =>
      a+" "+b
    }.map {
      case (year, text) => year+"\t"+text
    }

    persist(toTextFile(tsv, to, overwrite=true))
  }
}
