package rdrr.immutable

sealed trait RdfEntity { // Resources, Blank Nodes, Literals and Predicates
  def as[SubType](implicit converter: RdfEntity => SubType): SubType = converter(this)
}
sealed trait GraphNode extends RdfEntity // Resources, Blank Nodes and Literals
sealed trait RdfResource extends GraphNode // Resources and Blank Nodes

case class WrongNodeTypeException(message: String) extends Exception(message)


case class Resource(uri: String) extends RdfResource

object Resource {
  implicit def fromEntity(entity: RdfEntity): Resource = entity match {
    case resource: Resource => resource
    case somethingElse => throw new WrongNodeTypeException(s"Expected a Resource Node but got $somethingElse")
  }
}


case class Predicate(uri: String) extends RdfEntity

object Predicate {
  implicit def fromEntity(entity: RdfEntity): Predicate = entity match {
    case predicate: Predicate => predicate
    case somethingElse => throw new WrongNodeTypeException(s"Expected a Predicate Node but got $somethingElse")
  }
}


abstract class Literal extends GraphNode {
  def value: Any
  def datatype: Resource
}

object Literal {
  implicit def fromEntity(entity: RdfEntity): Literal = entity match {
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


case class BlankNode(label: String) extends RdfResource

object BlankNode {
  implicit def fromEntity(entity: RdfEntity): BlankNode = entity match {
    case blankNode: BlankNode => blankNode
    case somethingElse => throw new WrongNodeTypeException(s"Expected a Blank Node but got $somethingElse")
  }
}
