package chbrown

object ArgMapper {
  def apply(args: Seq[String], defaults: Map[String, String] = Map()) = {
    val flag = "-+(\\w+)".r
    val pairs = args.zip(args.drop(1) :+ "$$$")
    defaults ++ pairs.flatMap {
      case (flag(a), flag(b)) => List((a, "true"))
      case (flag(a), value) => List((a, value))
      case (a, "$$$") => List()
      case (a, b) => List(("wtf", a + b))
    }.toMap
  }
}

object Tabular {
  def read(filepath: String, headers: Option[List[String]] = None, field_sep: Char = '\t') = {
    // read from filepath and return a list of maps.
    // literal \t or , inside fields is not allowed at the moment
    val rows = io.Source.fromFile(filepath).getLines.map(
      _.split(field_sep).map(
        _.stripPrefix("\"").stripSuffix("\"").replaceAllLiterally("\"\"", "\"")
      )
    )
    val final_headers = headers match {
      case Some(some_headers) => some_headers
      case None => rows.next.toList
    }
    rows.map(cells => final_headers.zip(cells).toMap)
  }
}
