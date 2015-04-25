package rdrr.immutable


sealed abstract class Node {
  def as[SubType](implicit converter: Node => SubType): SubType = converter(this)
}

case class WrongNodeTypeException(message: String) extends Exception(message)


case class Resource(uri: String) extends Node

object Resource {
  implicit def fromNode(node: Node): Resource = node match {
    case resource: Resource => resource
    case somethingElse => throw new WrongNodeTypeException(s"Expected a Resource Node but got $somethingElse")
  }
}


case class Predicate(uri: String)


abstract class Literal extends Node {
  def value: Any
  def datatype: String
  def asString: String = value.toString
}

object Literal {
  implicit def fromNode(node: Node): Literal = node match {
    case literal: Literal => literal
    case somethingElse => throw new WrongNodeTypeException(s"Expected a Literal Node but got $somethingElse")
  }
}

abstract class StringLiteral extends Literal {
  def value: String
}
case class StandardStringLiteral(value: String) extends StringLiteral {
  val datatype = "http://www.w3.org/2001/XMLSchema#string"
}
case class LanguageStringLiteral(value: String, language: String) extends StringLiteral {
  val datatype = "http://www.w3.org/1999/02/22-rdf-syntax-ns#langString"
}
case class NonStandardStringLiteral(value: String, datatype: String) extends StringLiteral

case class BooleanLiteral(value: Boolean) extends Literal {
  val datatype = "http://www.w3.org/TR/xmlschema-2/#boolean"
}

case class IntegerLiteral(value: Int) extends Literal {
  val datatype = "http://www.w3.org/2001/XMLSchema#integer"
}

case class DecimalLiteral(value: Double) extends Literal {
  val datatype = "http://www.w3.org/2001/XMLSchema#decimal"
}
