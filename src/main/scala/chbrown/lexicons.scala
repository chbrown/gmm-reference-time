package chbrown

object Lexicon {
  lazy val liwcTrie = {
    ScalaJson.fromFile[Map[String, Any]]("/usr/local/data/liwc_2007.trie")
  }

  def walk(token: String, index: Int, cursor: Map[String, Any]): List[String] = {
    if (cursor.contains("*")) {
      // assert cursor("*") = List[String]
      return cursor("*").asInstanceOf[List[String]]
    }
    else if (cursor.contains("$") && index == token.size) {
      return cursor("$").asInstanceOf[List[String]]
    }
    else if (index < token.size) {
      var letter = token(index).toString
      if (cursor.contains(letter)) {
        val nextCursor = cursor(letter).asInstanceOf[Map[String, Any]]
        return walk(token, index + 1, nextCursor)
      }
    }
    return List()
  }

  def Liwc(tokens: Seq[String]): Map[String, Int] = {
    // document is a tokenized, lower-cased document
    val categories = tokens.map(walk(_, 0, liwcTrie))
    Map("Dic" -> categories.count(_.size > 0), "WC" -> tokens.size) ++
      categories.flatten.groupBy(identity).mapValues(_.size)
  }

  def LiwcLists(tokens: Seq[String]): Seq[List[String]] = {
    // document is a tokenized, lower-cased document
    tokens.map(walk(_, 0, liwcTrie))
  }
}
