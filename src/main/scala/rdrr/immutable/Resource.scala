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
  val datatype = "http://www.w3.org/1999/02/22-rdf-syntax-ns#langString"
}
case class NonStandardStringLiteral(value: String, datatype: String) extends Literal


case class BooleanLiteral(boolean: Boolean) extends Literal {
  override val value = boolean.toString
  override val datatype = "http://www.w3.org/TR/xmlschema-2/#boolean"
}


case class IntegerLiteral(integer: Int) extends Literal {
  override val value = integer.toString
  override val datatype = "http://www.w3.org/2001/XMLSchema#integer"
}

case class DecimalLiteral(decimal: Double) extends Literal {
  override val value = decimal.toString
  override val datatype = "http://www.w3.org/2001/XMLSchema#decimal"
}