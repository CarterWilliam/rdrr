package rdrr.immutable.marshallers

import rdrr.immutable.Graph

trait GraphMarshaller {
  def fromTurtle(turtle: String): Graph
  def toTurtle(graph: Graph): String
}
