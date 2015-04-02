package jesc

import java.io.{StringWriter, StringReader}
import com.hp.hpl.jena.rdf.model.{ModelFactory, Model => JenaModel}
import scala.collection.JavaConverters._


case class Graph(model: JenaModel) extends JavaHelpers {
  lazy val subjects: Stream[Resource] = model.listSubjects.asScala.toStream.map(Resource(_))
  lazy val toTurtle: String = using(new StringWriter) { out =>
    model.write(out, "TTL")
    out.toString
  }
}

object Graph extends JavaHelpers {
  def parse(turtle: String): Graph = using(new StringReader(turtle)) { reader =>
    val model = ModelFactory.createDefaultModel()
    model.read(reader, null, "TTL")
    Graph(model)
  }
}

trait JavaHelpers {
  def using[T <: { def close(): Unit }, R](closable: T)(operation: T => R) = {
    val result = operation(closable)
    closable.close()
    result
  }
}
