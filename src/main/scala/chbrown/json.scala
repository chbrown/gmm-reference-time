package chbrown

import com.codahale.jerkson.Json.parse
import scala.collection.JavaConverters._

object ScalaJson {
  def scalafy(entity: Any): Any = {
    // thanks: http://stackoverflow.com/questions/674713/converting-java-collection-into-scala-collection
    // import scala.collection.JavaConverters._ // asScala
    entity match {
      case obj: java.util.LinkedHashMap[_, _] =>
        obj.asScala.toMap.mapValues(scalafy)
      case arr: java.util.ArrayList[_] =>
        arr.asScala.toList.map(scalafy)
      case x => x
    }
  }

  def fromFile[A](path: String): A = {
    val raw = io.Source.fromFile(path).mkString
    scalafy(parse(raw)).asInstanceOf[A]
  }
}
