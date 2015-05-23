package rdrr.immutable.marshallers

import java.io.{StringReader, StringWriter}

import com.hp.hpl.jena.rdf.model.{ModelFactory, Literal => JenaLiteral, Property => JenaProperty, RDFNode => JenaNode, Resource => JenaResource, Statement => JenaStatement}
import rdrr.immutable._
import rdrr.util.JavaHelpers

import scala.collection.JavaConverters._


class JenaTurtleUnmarshaller extends TurtleUnmarshaller with JavaHelpers {

  override def fromTurtle(turtle: String): Graph = closeWhenDone(new StringReader(turtle)) { reader =>
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
  private def literalFromJena(literal: JenaLiteral) = StandardStringLiteral(literal.asLiteral().getValue.toString)



}


class JenaTurtleMarshaller extends TurtleMarshaller with JavaHelpers {

  override def toTurtle(graph: Graph): String = {

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

    closeWhenDone(new StringWriter) { out =>
      graph.foreach { triple =>
        jenaModel.add(triple.subject, triple.predicate, triple.`object`)
      }
      jenaModel.write(out, "TTL")
      out.toString
    }

  }

}
