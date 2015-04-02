package jesc

import org.specs2.mutable.Specification
import org.specs2.specification.Scope
import utilities.TestHelpers

class GraphSpec extends Specification {

  "Graphs" can {
    "be parsed from a turtle document string" in new GraphScope {
      val graph = Graph.parse(bieberTurtle)
      graph.model.isEmpty must beFalse
    }
    "be output as a turtle document string" in new GraphScope {
      // Cannot rely on ordering of prefixes and triples so base equality on length
      val graph = Graph.parse(bieberTurtle)
      val inputLength = stripWhitespace(graph.toTurtle).length
      val outputLength = stripWhitespace(bieberTurtle).length
      inputLength must be equalTo outputLength
    }
  }

}

trait GraphScope extends Scope with TestHelpers {
  val bieberTurtle = getResource("bieber.ttl")
  def stripWhitespace(string: String) = string.replaceAll("\\s", "")
}
