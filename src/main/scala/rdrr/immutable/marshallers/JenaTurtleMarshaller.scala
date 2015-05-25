package rdrr.immutable.marshallers

import java.io.{StringReader, StringWriter}

import com.hp.hpl.jena.rdf.model.{Literal => JenaLiteral, Property => JenaProperty, RDFNode => JenaNode, Resource => JenaResource, Statement => JenaStatement, AnonId, ModelFactory}
import rdrr.immutable._
import rdrr.util.JavaHelpers

import scala.collection.JavaConverters._


object JenaTurtleUnmarshaller extends TurtleUnmarshaller with JavaHelpers {

  override def fromTurtle(turtle: String): Graph = closeWhenDone(new StringReader(turtle)) { reader =>
    val model = ModelFactory.createDefaultModel()
    model.read(reader, null, "TTL")
    val triples = tripleStreamFromJenaStatements(model.listStatements.asScala.toStream)
    Graph(triples)
  }

  private def tripleStreamFromJenaStatements(statementStream: Stream[JenaStatement]): Stream[Triple] = statementStream match {
    case statement #:: others => {
      val subject = resourceFromJena(statement.getSubject)
      val predicate = Predicate(statement.getPredicate.getURI)
      val `object` = objectFromJena(statement.getObject)
      Triple(subject, predicate, `object`) #:: tripleStreamFromJenaStatements(others)
    }
    case Stream.Empty => Stream.Empty
  }

  private def resourceFromJena(jenaResource: JenaResource) = jenaResource match {
    case blankNode if jenaResource.isAnon => BlankNode(jenaResource.getId.getLabelString)
    case resource => Resource(resource.getURI)
  }

  private def objectFromJena(node: JenaNode): GraphNode = node match {
    case literalNode if literalNode.isLiteral => literalFromJena(literalNode.asLiteral())
    case resourceNode => resourceFromJena(resourceNode.asResource)
  }

  private def literalFromJena(literal: JenaLiteral) = StandardStringLiteral(literal.asLiteral().getValue.toString)

}


object JenaTurtleMarshaller extends TurtleMarshaller with JavaHelpers {

  override def toTurtle(graph: Graph): String = {

    val jenaModel = ModelFactory.createDefaultModel()

    implicit def resourceJenaConversion(resource: RdfResource): JenaResource = resource match {
      case resource: Resource => jenaModel.createResource(resource.uri)
      case blankNode: BlankNode => jenaModel.createResource(new AnonId(blankNode.label))
    }
    implicit def predicateJenaConversion(predicate: Predicate): JenaProperty = {
      jenaModel.createProperty(predicate.uri)
    }

    implicit def nodeJenaConversion(node: GraphNode): JenaNode = node match {
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
