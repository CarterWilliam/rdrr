package jesc.immutable

import java.io.{StringReader, StringWriter}
import com.hp.hpl.jena.rdf.model.{ModelFactory, Property => JenaProperty, RDFNode => JenaNode, Resource => JenaResource}
import jesc.JavaHelpers
import scala.collection.JavaConverters._


case class Graph(triples: List[Triple]) extends JavaHelpers {
  
  val jenaModel = ModelFactory.createDefaultModel()
  lazy val subjects: List[Resource] = triples.map(_.subject)

  def +(t: Triple): Graph = Graph(t :: triples)

  def toTurtle: String = using(new StringWriter) { out =>
    triples.foreach { triple =>
      jenaModel.add(triple.subject, triple.predicate, triple.`object`)
    }
    jenaModel.write(out, "TTL")
    out.toString
  }

  implicit def resourceJenaConversion(resource: Resource): JenaResource = {
    jenaModel.createResource(resource.uri)
  }
  implicit def predicateJenaConversion(predicate: Predicate): JenaProperty = {
    jenaModel.createProperty(predicate.uri)
  }
  implicit def nodeJenaConversion(node: Node): JenaNode = {
    node match {
      case resource: Resource => resourceJenaConversion(resource)
      case _ => jenaModel.createLiteral("literal")
    }
  }

}

object Graph extends JavaHelpers {

  val Empty = Graph(Nil)

  def parse(turtle: String): Graph = using(new StringReader(turtle)) { reader =>
    val model = ModelFactory.createDefaultModel()
    model.read(reader, null, "TTL")
    model.listStatements.asScala.foldLeft(Empty) { case (graph, statement) =>
      graph + Triple(Resource(statement.getSubject), Predicate(statement.getPredicate), Node(statement.getObject))
    }
  }
}
