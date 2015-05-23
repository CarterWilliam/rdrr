package rdrr.immutable.marshallers

import rdrr.immutable.RdfGraph

trait TurtleMarshaller {
  def toTurtle(graph: RdfGraph): String
}

trait TurtleUnmarshaller {
  def fromTurtle(turtle: String): RdfGraph
}

case class TurtleParseException(message: String) extends Exception(message)
