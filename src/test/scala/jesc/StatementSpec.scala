package jesc

import org.specs2.mutable.Specification
import org.specs2.specification.Scope
import utilities.TestHelpers

class StatementSpec extends Specification {

  "Statements" should {
    "have a subject resource" in new StatementScope {
      bieberIsAnArtist.subject.uri must be equalTo "https://en.wikipedia.org/wiki/Justin_Bieber"
    }
    "have a predicate resource" in new StatementScope {
      bieberIsAnArtist.predicate.uri must be equalTo "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"
    }
    "have an object resource" in new StatementScope {
      bieberIsAnArtist.`object`.value must be equalTo "http://purl.org/ontology/mo/MusicArtist"
    }
  }
}

trait StatementScope extends Scope with TestHelpers {
  val graph = Graph.parse(getResource("bieber-single-statement.ttl"))
  val bieberIsAnArtist = graph.subjects.head.statements.head
}
