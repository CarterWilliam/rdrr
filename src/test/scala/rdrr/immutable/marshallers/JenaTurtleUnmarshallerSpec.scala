package rdrr.immutable.marshallers

import org.specs2.mutable.Specification
import org.specs2.specification.Scope
import rdrr.immutable.BlankNode
import utilities.TestHelpers

class JenaTurtleUnmarshallerSpec extends Specification {

  "The Jena Unmarshaller" can {

    "serialise from Turtle format" in new JenaTurtleUnmarshallerScope {
      val turtle = getResource("bieber.ttl")
      val graph = unmarshaller.fromTurtle(turtle)
      graph must have size 3
    }

    "be able to handle labelled blank nodes" in new Scope with TestHelpers {
      // The Jena marshaller does not seem to extract the labels given in the turtle
      val blankNodeTurtle = getResource("labeled-blank-nodes.ttl")
      val graph = JenaTurtleUnmarshaller.fromTurtle(blankNodeTurtle)
      (graph(0).subject, graph(1).`object`) must beLike {
        case (BlankNode(label1), BlankNode(label2)) => label1 must be equalTo label2
      }
    }

  }
}

trait JenaTurtleUnmarshallerScope extends Scope with TestHelpers {
  val unmarshaller = JenaTurtleUnmarshaller
}

