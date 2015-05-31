package rdrr.immutable.marshallers

import org.specs2.mutable.Specification
import org.specs2.specification.Scope
import rdrr.immutable.BlankNode
import utilities.TestHelpers

class JenaTurtleUnmarshallerSpec extends Specification {

  "The Jena Unmarshaller" can {
    "serialise from Turtle format" in new JenaTurtleUnmarshallerScope {
      val turtle =
        """
          |@prefix mo: <http://purl.org/ontology/mo/> .
          |<https://en.wikipedia.org/wiki/Justin_Bieber>
          |    a mo:MusicArtist , mo:SoloMusicArtist ;
          |    <http://xmlns.com/foaf/0.1/name> "Justin Bieber"@en .
        """.stripMargin
      val graph = unmarshaller.fromTurtle(turtle)
      graph must have size 3
    }

    "be able to handle labelled blank nodes" in new JenaTurtleUnmarshallerScope {
      val blankNodeTurtle =
        """
          |@prefix foaf: <http://xmlns.com/foaf/0.1/> .
          |_:alice foaf:knows _:bob .
          |_:bob foaf:knows _:alice .
        """.stripMargin
      val graph = unmarshaller.fromTurtle(blankNodeTurtle)
      (graph(0).subject, graph(1).`object`) must beLike {
        case (BlankNode(label1), BlankNode(label2)) => label1 must be equalTo label2
      }
    }

    "handle a large turtle file" in new JenaTurtleUnmarshallerScope {
      val turtle = getResource("bbc.ttl")
      val graph = unmarshaller.fromTurtle(turtle)
      graph.size must be equalTo 287
    }


  }
}

trait JenaTurtleUnmarshallerScope extends Scope with TestHelpers {
  val unmarshaller = JenaTurtleUnmarshaller
}

