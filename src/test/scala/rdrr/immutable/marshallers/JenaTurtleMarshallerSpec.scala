package rdrr.immutable.marshallers

import rdrr.immutable._
import org.specs2.mutable.Specification
import org.specs2.specification.Scope
import utilities.TestHelpers

class JenaTurtleMarshallerSpec extends Specification {

  "The Jena Marshaller" can {
    "serialise from Turtle format" in new JenaTurtleMarshallerScope {
      val turtle = getResource("bieber.ttl")
      val graph = serialiser.fromTurtle(turtle)
      graph must have size 3
    }
    "serialise to Turtle format" in new JenaTurtleMarshallerScope {
      val graph = Graph(Triple(
        Resource("https://en.wikipedia.org/wiki/Justin_Bieber"),
        Predicate("http://www.w3.org/1999/02/22-rdf-syntax-ns#type"),
        Resource("http://purl.org/ontology/mo/MusicArtist") ) )
      val expected = "<https://en.wikipedia.org/wiki/Justin_Bieber> a <http://purl.org/ontology/mo/MusicArtist> ."
      serialiser.toTurtle(graph) must beEqualTo (expected).ignoreSpace
    }
  }
}

trait JenaTurtleMarshallerScope extends Scope with TestHelpers {
  val serialiser = new JenaTurtleMarshaller()
}
