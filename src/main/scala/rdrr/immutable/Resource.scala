package rdrr.immutable

sealed trait RdfEntity
sealed trait GraphNode extends RdfEntity {
  def as[SubType](implicit converter: GraphNode => SubType): SubType = converter(this)
}

case class WrongNodeTypeException(message: String) extends Exception(message)


case class Resource(uri: String) extends GraphNode

object Resource {
  implicit def fromNode(node: GraphNode): Resource = node match {
    case resource: Resource => resource
    case somethingElse => throw new WrongNodeTypeException(s"Expected a Resource Node but got $somethingElse")
  }
}


case class Predicate(uri: String) extends RdfEntity


abstract class Literal extends GraphNode {
  def value: Any
  def datatype: Resource
}

object Literal {
  implicit def fromNode(node: GraphNode): Literal = node match {
    case literal: Literal => literal
    case somethingElse => throw new WrongNodeTypeException(s"Expected a Literal Node but got $somethingElse")
  }
}

abstract class StringLiteral extends Literal {
  def value: String
}
case class StandardStringLiteral(value: String) extends StringLiteral {
  val datatype = Resource("http://www.w3.org/2001/XMLSchema#string")
}
case class LanguageStringLiteral(value: String, language: String) extends StringLiteral {
  val datatype = Resource("http://www.w3.org/1999/02/22-rdf-syntax-ns#langString")
}
case class NonStandardStringLiteral(value: String, datatype: Resource) extends StringLiteral
object NonStandardStringLiteral {
  def apply(value: String, datatypeUri: String): NonStandardStringLiteral =
    NonStandardStringLiteral(value, Resource(datatypeUri))
}

case class BooleanLiteral(value: Boolean) extends Literal {
  val datatype = Resource("http://www.w3.org/TR/xmlschema-2/#boolean")
}

case class IntegerLiteral(value: Int) extends Literal {
  val datatype = Resource("http://www.w3.org/2001/XMLSchema#integer")
}

case class DecimalLiteral(value: Double) extends Literal {
  val datatype = Resource("http://www.w3.org/2001/XMLSchema#decimal")
}
