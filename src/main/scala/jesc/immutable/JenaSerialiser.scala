package jesc.immutable

import java.io.{StringReader, StringWriter}

import com.hp.hpl.jena.rdf.model.{ModelFactory, Property => JenaProperty, RDFNode => JenaNode, Resource => JenaResource, Literal => JenaLiteral, Statement => JenaStatement}
import jesc.util.JavaHelpers

import scala.collection.JavaConverters._


trait GraphSerialiser {
  def fromTurtle(turtle: String): Graph
  def toTurtle(graph: Graph): String
}

class JenaSerialiser extends GraphSerialiser with JavaHelpers {

  override def fromTurtle(turtle: String): Graph = using(new StringReader(turtle)) { reader =>
    val model = ModelFactory.createDefaultModel()
    model.read(reader, null, "TTL")
    val triples = tripleStreamFromJenaStatements(model.listStatements.asScala.toStream)
    Graph(triples)
  }

  private def tripleStreamFromJenaStatements(statementStream: Stream[JenaStatement]): Stream[Triple] = statementStream match {
    case statement #:: others => {
      val subject = Resource(statement.getSubject.getURI)
      val predicate = Predicate(statement.getPredicate.getURI)
      val `object` = objectFromJena(statement.getObject)
      Triple(subject, predicate, `object`) #:: tripleStreamFromJenaStatements(others)
    }
    case Stream.Empty => Stream.Empty
  }

  private def objectFromJena(node: JenaNode) = node match {
    case literal if literal.isLiteral => literalFromJena(literal.asLiteral())
    case resource => Resource(resource.asResource.getURI)
  }
  private def literalFromJena(literal: JenaLiteral) = StringLiteral(literal.asLiteral().getValue.toString)


  override def toTurtle(graph: Graph): String = using(new StringWriter) { out =>

    val jenaModel = ModelFactory.createDefaultModel()

    implicit def resourceJenaConversion(resource: Resource): JenaResource = {
      jenaModel.createResource(resource.uri)
    }
    implicit def predicateJenaConversion(predicate: Predicate): JenaProperty = {
      jenaModel.createProperty(predicate.uri)
    }

    implicit def nodeJenaConversion(node: Node): JenaNode = node match {
      case resource: Resource => resourceJenaConversion(resource)
      case _ => jenaModel.createLiteral("literal")
    }

    graph.triples.foreach { triple =>
      jenaModel.add(triple.subject, triple.predicate, triple.`object`)
    }
    jenaModel.write(out, "TTL")
    out.toString
  }


}
