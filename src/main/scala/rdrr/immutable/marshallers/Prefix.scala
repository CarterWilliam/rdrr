package rdrr.immutable.marshallers

sealed trait RdfPrefix {
  def path: String
}
case class Prefix(prefix: String, path: String) extends RdfPrefix
case class BasePrefix(path: String) extends RdfPrefix
