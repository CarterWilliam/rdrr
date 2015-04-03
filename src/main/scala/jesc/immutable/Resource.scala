package jesc.immutable


case class Triple(subject: Resource, predicate: Predicate, `object`: Node)

abstract class Node

case class Resource(uri: String) extends Node

case class Predicate(uri: String) extends Node

abstract class Literal extends Node {
  def value: String
}

case class StringLiteral(value: String, language: String = "en") extends Literal