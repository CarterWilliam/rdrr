package jesc

import org.specs2.mutable.Specification
import org.specs2.specification.Scope
import utilities.TestHelpers

class ResourceSpec extends Specification {

  "Resources" can {
    "be created from a URI string" in {
      Resource("http://purl.org/ontology/mo/MusicArtist").value must be equalTo "http://purl.org/ontology/mo/MusicArtist"
    }
  }

  "Resources" should {
    "exist on a graph object" in new ResourceScope {
      graph.subjects must have size 1
    }
    "have a URI" in new ResourceScope {
      val subject = graph.subjects.head
      subject.uri must be equalTo "https://en.wikipedia.org/wiki/Justin_Bieber"
    }
    "contain a subset of subject, predicate, object statements from the model" in new ResourceScope {
      val subject = graph.subjects.head
      subject.statements must have size 3
    }
    "have a notion of equality" in new ResourceScope {
      val subject = graph.subjects.head
      val copy = Resource("https://en.wikipedia.org/wiki/Justin_Bieber")
      subject must be equalTo copy
    }
  }

}

trait ResourceScope extends Scope with TestHelpers {
  val graph = Graph.parse(getResource("bieber.ttl"))
}
