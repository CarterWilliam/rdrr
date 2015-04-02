package jesc

import java.io.StringReader

import com.hp.hpl.jena.rdf.model.{ModelFactory, Literal => JenaLiteral, Model => JenaModel, Property => JenaProperty, RDFNode => JenaNode, Resource => JenaResource, Statement => JenaStatement}

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


case class Resource(resource: JenaResource) extends RdfNode {
  lazy val uri = resource.getURI
  lazy val statements: Stream[Statement] = resource.listProperties.asScala.toStream.map(Statement)

  override lazy val value = uri
}

object Resource {
  def apply(uri: String): Resource =
    Resource(ModelFactory.createDefaultModel().createResource(uri))
}

case class Predicate(property: JenaProperty) extends RdfNode {
  lazy val uri = property.getURI

  override lazy val value = uri
}

object Predicate {
  def apply(uri: String): Predicate =
    Predicate(ModelFactory.createDefaultModel().createProperty(uri))
}

case class Literal(literal: JenaLiteral) extends RdfNode {
  override lazy val value = literal.getValue.toString
}

case class Statement(statement: JenaStatement) {
  lazy val subject = Resource(statement.getSubject)
  lazy val predicate = Predicate(statement.getPredicate)
  lazy val `object` = RdfNode(statement.getObject)
}

abstract class RdfNode {
  def value: String
}

object RdfNode {
  def apply(jenaNode: JenaNode) = {
    if (jenaNode.isLiteral)
      Literal(jenaNode.asLiteral())
    else
      Resource(jenaNode.asResource())
  }
}
