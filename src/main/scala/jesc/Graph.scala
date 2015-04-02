package jesc

import java.io.StringReader
import com.hp.hpl.jena.rdf.model.{ModelFactory, Model => JenaModel}
import scala.collection.JavaConverters._


case class Graph(model: JenaModel) {
  lazy val subjects: Stream[Resource] = model.listSubjects.asScala.toStream.map(Resource(_))
}

object Graph {
  def parse(turtle: String): Graph = {
    
    val model = ModelFactory.createDefaultModel()

    val stringReader = new StringReader(turtle)
    try {
      model.read(stringReader, null, "TTL")
    } finally {
      stringReader.close()
    }

    Graph(model)
  }
}
