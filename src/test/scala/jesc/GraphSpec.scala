package jesc

import org.specs2.mutable.Specification
import org.specs2.specification.Scope
import utilities.TestHelpers

class GraphSpec extends Specification {

  "Graphs" can {
    "be parsed from a turtle document string" in new GraphScope {
      val graph = Graph.parse(beiberTurtle)
      graph.model.isEmpty must beFalse
    }
  }

}

trait GraphScope extends Scope with TestHelpers {
  val beiberTurtle = getResource("bieber.ttl")
}
