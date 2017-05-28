package rdrr.immutable.marshallers

import rdrr.immutable._
import org.specs2.mutable.Specification
import org.specs2.specification.Scope
import utilities.TestHelpers

class JenaTurtleMarshallerSpec extends Specification {

  "The Jena Marshaller" can {

    "output the empty graph" in new JenaTurtleMarshallerScope {
      marshaller.toTurtle(Graph.Empty) must be equalTo ""
    }

    "serialise to Turtle format" in new JenaTurtleMarshallerScope {
      val graph = Graph(Triple(
        Resource("https://en.wikipedia.org/wiki/Justin_Bieber"),
        Resource("http://www.w3.org/1999/02/22-rdf-syntax-ns#type"),
        Resource("http://purl.org/ontology/mo/MusicArtist") ) )
      val expected = "<https://en.wikipedia.org/wiki/Justin_Bieber> a <http://purl.org/ontology/mo/MusicArtist> ."
      marshaller.toTurtle(graph) must beEqualTo (expected).ignoreSpace
    }


    "serialise blank nodes to turtle" in new JenaTurtleMarshallerScope {
      val graph = Graph(Triple(
        BlankNode("something"),
        Resource("http://www.w3.org/1999/02/22-rdf-syntax-ns#type"),
        Resource("http://purl.org/ontology/mo/MusicArtist") ) )

      val expected = "[] a <http://purl.org/ontology/mo/MusicArtist> ."
      marshaller.toTurtle(graph) must beEqualTo (expected).ignoreSpace
    }

  }
}

trait JenaTurtleMarshallerScope extends Scope {
  val marshaller = JenaTurtleMarshaller
}
