package jesc.immutable

import com.hp.hpl.jena.rdf.model.{Property => JenaProperty, RDFNode => JenaNode, Resource => JenaResource, Literal => JenaLiteral}

case class Triple(subject: Resource, predicate: Predicate, `object`: Node)

abstract class Node

object Node {
  def apply(jenaNode: JenaNode): Node = {
    if (jenaNode.isLiteral)
      Literal(jenaNode.asLiteral())
    else
      Resource(jenaNode.asResource())
  }
}

case class Resource(uri: String) extends Node

object Resource {
  def apply(jenaResource: JenaResource): Resource = Resource(jenaResource.getURI)
}

case class Predicate(uri: String) extends Node

object Predicate {
  def apply(jenaPredicate: JenaProperty): Predicate = Predicate(jenaPredicate.getURI)
}

abstract class Literal extends Node {
  def value: String
}

object Literal {
  def apply(jenaLiteral: JenaLiteral): Literal = {
    StringLiteral(jenaLiteral.getValue.toString)
  }
}

case class StringLiteral(value: String, language: String = "en") extends Literal