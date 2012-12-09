package chbrown.tacc

import com.nicta.scoobi.Scoobi._
import org.apache.hadoop.fs.Path

object DtmPrep extends ScoobiApp {
  // note to self: stopwords cannot be out here, it must be in teh run method.

  def writeHdfsFile(path: String, contents: String, overwrite: Boolean = false) {
    // lookup_out is a FSDataOutputStream
    val lookup_out = configuration.fs.create(new Path(path), overwrite)
    // while ((bytesRead = in.read(buffer)) > 0) {
    //   out.write(buffer, 0, bytesRead);
    // }
    val bytes = contents.getBytes("UTF-8")
    lookup_out.write(bytes)
    lookup_out.close()
  }

  def run() {
    val stopwords =  "a b c d e f g h j k l m n o p q r s t u v w x y z also all about who have not has th had been be its this or an but are that were as which with it from on at for is by was to and in of the".split(' ').toSet

    // run chbrown.tacc.DtmPrep eb-12k-windows.tsv eb-12k-windows-svm
    var from = args(0)
    val to = args(1)

    val tokens = fromDelimitedTextFile(from, "\t") {
      case year :: text :: _ => text
      // case Array(year, text) => text
    } flatMap { text =>
      "[a-z]['a-z]*".r.findAllIn(text.toLowerCase).toList.filterNot(stopwords).map((_, 1))
    }

    val distributed_counts = tokens.groupByKey.combine((a: Int, b: Int) => a + b)

    // val strs = counts.map { case (token, count) => count + "\t" + token }
    // persist(toTextFile(strs, to, overwrite=true))

    val counts = persist(distributed_counts.materialize).toList
    // counts is List((token: String, count: Int))
    // sort descending on counts and then drop counts. most frequest words will have shortest indices=keys.
    val types = counts.sortBy(_._2).reverse.map(_._1)
    // zipWithIndex: immutable.Seq[(A, Int)]
    val typeIndices = types.zipWithIndex

    // print out as "0,word"
    val lookup_content = typeIndices.map(type_index => type_index._2+","+type_index._1).mkString("\n")
    writeHdfsFile(to+"-lookup", lookup_content, overwrite=true)

    val typeIndexMap = typeIndices.toMap

    val svm_lines = fromDelimitedTextFile(from, "\t") {
      case year :: text :: _ => text
        val tokens = "[a-z]['a-z]*".r.findAllIn(text.toLowerCase).toList.filterNot(stopwords)
        val indices = tokens.map(typeIndexMap)
        val indexCounts = indices.groupBy(identity).toList.sortBy(_._1).map { case (k, v) =>
          k+":"+v.length
        }
        year+"\t"+indexCounts.size+" "+indexCounts.mkString(" ")
    }

    // val words = fromDelimitedTextFile(from, "\t") {
    //   case year :: text :: _ => text
    //   // case Array(year, text) => text
    // } map { text =>
    //   "[^'a-z0-9A-Z]+".r.findAllIn(text.toLowerCase).toList.map((_, 1))
    // }

    persist(toTextFile(svm_lines, to, overwrite=true))
  }
}


 // (a) foo-mult.dat, which is one-doc-per-line, each line of the form

 //   unique_word_count index1:count1 index2:count2 ... indexn:counnt

 //   where each index is an integer corresponding to a unique word.

 // (b) foo-seq.dat, which is of the form

 //   Number_Timestamps
 //   number_docs_time_1
 //   ...
 //   number_docs_time_i
 //   ...
 //   number_docs_time_NumberTimestamps

 //   - The docs in foo-mult.dat should be ordered by date, with the first
 //     docs from time1, the next from time2, ..., and the last docs from
 //     timen.
