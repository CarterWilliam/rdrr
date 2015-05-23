package rdrr.immutable.marshallers

import rdrr.immutable.Graph

trait TurtleMarshaller {
  def toTurtle(graph: Graph): String
}

trait TurtleUnmarshaller {
  def fromTurtle(turtle: String): Graph
}

case class TurtleParseException(message: String) extends Exception(message)
