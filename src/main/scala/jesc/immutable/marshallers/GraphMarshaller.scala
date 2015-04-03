package jesc.immutable.marshallers

import jesc.immutable.Graph

trait GraphMarshaller {
  def fromTurtle(turtle: String): Graph
  def toTurtle(graph: Graph): String
}
