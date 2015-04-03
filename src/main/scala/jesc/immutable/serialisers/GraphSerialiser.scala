package jesc.immutable.serialisers

import jesc.immutable.Graph

trait GraphSerialiser {
  def fromTurtle(turtle: String): Graph
  def toTurtle(graph: Graph): String
}
