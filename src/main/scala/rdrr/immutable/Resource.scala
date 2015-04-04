package rdrr.immutable


case class Triple(subject: Resource, predicate: Predicate, `object`: Node)

abstract class Node

case class Resource(uri: String) extends Node

case class Predicate(uri: String) extends Node

abstract class Literal extends Node {
  def value: String
  def datatype: String
}

case class StringLiteral(value: String) extends Literal {
  val datatype = "http://www.w3.org/2001/XMLSchema#string"
}
case class LanguageStringLiteral(value: String, language: String) extends Literal {
  val datatype: String = "http://www.w3.org/1999/02/22-rdf-syntax-ns#langString"
}
case class NonStandardStringLiteral(value: String, datatype: String) extends Literal
