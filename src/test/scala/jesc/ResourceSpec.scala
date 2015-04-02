package jesc

import org.specs2.mutable.Specification
import org.specs2.specification.Scope
import utilities.TestHelpers

class ResourceSpec extends Specification {

  "Resources" should {
    "exist on a graph object" in new ResourceScope {
      graph.subjects must have size 1
    }
    "have a URI" in new ResourceScope {
      val subject = graph.subjects.head
      subject.value must be equalTo "https://en.wikipedia.org/wiki/Justin_Bieber"
    }
    "contain a subset of subject, predicate, object statements from the model" in new ResourceScope {
      val subject = graph.subjects.head
      subject.statements must haveSize (2)
    }
  }

}

trait ResourceScope extends Scope with TestHelpers {
  val graph = Graph.parse(getResource("bieber.ttl"))
}
