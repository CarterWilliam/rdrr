package jesc.immutable

import org.specs2.mutable.Specification
import org.specs2.specification.Scope
import utilities.TestHelpers

class JenaSerialiserSpec extends Specification {

  "The Jena Serialiser" can {
    "serialise from Turtle format" in new SerialiserScope {
      val turtle = getResource("bieber.ttl")
      val graph = serialiser.fromTurtle(turtle)
      graph.triples must have size 3
    }
    "serialise to Turtle format" in new SerialiserScope {
      val graph = Graph.Empty + Triple (
        Resource("https://en.wikipedia.org/wiki/Justin_Bieber"),
        Predicate("http://www.w3.org/1999/02/22-rdf-syntax-ns#type"),
        Resource("http://purl.org/ontology/mo/MusicArtist") )
      val expected = "<https://en.wikipedia.org/wiki/Justin_Bieber> a <http://purl.org/ontology/mo/MusicArtist> ."
      serialiser.toTurtle(graph) must beEqualTo (expected).ignoreSpace
    }
  }
}

trait SerialiserScope extends Scope with TestHelpers {
  val serialiser = new JenaSerialiser()
}
