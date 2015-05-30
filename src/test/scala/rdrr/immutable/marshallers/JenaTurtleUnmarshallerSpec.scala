package rdrr.immutable.marshallers

import org.specs2.mutable.Specification
import org.specs2.specification.Scope
import utilities.TestHelpers

class JenaTurtleUnmarshallerSpec extends Specification {

  "The Jena Unmarshaller" can {
    "serialise from Turtle format" in new JenaTurtleUnmarshallerScope {
      val turtle = getResource("bieber.ttl")
      val graph = unmarshaller.fromTurtle(turtle)
      graph must have size 3
    }
  }
}

trait JenaTurtleUnmarshallerScope extends Scope with TestHelpers {
  val unmarshaller = JenaTurtleUnmarshaller
}

