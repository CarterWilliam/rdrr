package rdrr

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
    "return object as a resource if appropriate" in new StatementScope {
      bieberIsAnArtist.`object` must be equalTo Resource("http://purl.org/ontology/mo/MusicArtist")
    }
    "return object as a literal if appropriate" in new StatementScope {
      bieberHasNameJustinBieber.`object` must be equalTo Literal("Justin Bieber", "en")
    }
  }
}

trait StatementScope extends Scope with TestHelpers {
  val bieberIsAnArtist: Statement =
    Graph.parse(getResource("bieber-is-an-artist.ttl")).subjects.head.statements.head


  val bieberHasNameJustinBieber: Statement =
    Graph.parse(getResource("bieber-has-name-justin-bieber.ttl")).subjects.head.statements.head
}
