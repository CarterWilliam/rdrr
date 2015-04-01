package jess

import java.io.StringReader

import scala.collection.JavaConverters._
import com.hp.hpl.jena.rdf.model.{
  Model => JenaModel,
  Resource => JenaResource,
  Statement => JenaStatement,
  Property => JenaProperty,
  RDFNode => JenaNode,
  ModelFactory }


case class Graph(model: JenaModel) {
  lazy val subjects: Stream[Resource] = model.listSubjects.asScala.toStream.map(Resource)
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

case class Resource(resource: JenaResource) {
  lazy val uri = resource.getURI
  lazy val statements: Stream[Statement] = resource.listProperties.asScala.toStream.map(Statement)
}

case class Statement(statement: JenaStatement) {
  lazy val subject: JenaResource = statement.getSubject
  lazy val predicate: JenaProperty = statement.getPredicate
  lazy val `object`: JenaNode = statement.getObject
}

class JessException(message: String) extends Exception(message)
