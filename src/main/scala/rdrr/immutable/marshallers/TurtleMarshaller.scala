package rdrr.immutable.marshallers

import rdrr.immutable.Graph

trait TurtleMarshaller {
  def fromTurtle(turtle: String): Graph
  def toTurtle(graph: Graph): String
}

case class TurtleParseError(message: String) extends Exception(message)
