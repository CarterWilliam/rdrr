import jesc.Graph
import org.specs2.mutable.Specification
import org.specs2.specification.Scope

import scala.io.Source

class RdfDocumentSpec extends Specification {

  "Graphs" can {
    "be parsed from a turtle document string" in new TestScope {
      val graph = Graph.parse(beiberTurtle)
      graph.model.isEmpty must beFalse
    }
  }

  "Resources" should {
    "exist on a graph object" in new TestScope {
      val graph = Graph.parse(beiberTurtle)
      graph.subjects must have size 1
    }
    "have a URL" in new TestScope {
      val graph = Graph.parse(beiberTurtle)
      graph.subjects.head.uri must be equalTo "https://en.wikipedia.org/wiki/Justin_Bieber"
    }
    "contain a subset of subject, predicate, object statements from the model" in new TestScope {
      val graph = Graph.parse(beiberTurtle)
      graph.subjects.head.statements must haveSize (2)
    }
  }

}

trait TestScope extends Scope {
  val beiberTurtle = Source.fromURL(getClass.getResource("bieber.ttl")).mkString
}
