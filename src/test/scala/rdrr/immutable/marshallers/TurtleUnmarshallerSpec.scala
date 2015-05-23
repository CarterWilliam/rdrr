package rdrr.immutable.marshallers

import org.specs2.mutable.Specification
import org.specs2.specification.Scope
import utilities.TestHelpers

class TurtleUnmarshallerSpec extends Specification {

  case class MarshallerImplementation(name: String, marshaller: TurtleUnmarshaller)

  val rdrrImplementation = MarshallerImplementation("RDRR", new RdrrTurtleUnmarshaller)
  val jenaImplementation = MarshallerImplementation("Jena", new JenaTurtleUnmarshaller)

  Seq(jenaImplementation, rdrrImplementation).foreach { implementation =>

    val marshaller = implementation.marshaller

    s"The ${implementation.name} turtle marshaller" should {
      "be able to handle a large turtle file" in new Scope with TestHelpers {
        val turtle = getResource("bbc.ttl")
        val graph = marshaller.fromTurtle(turtle)
        graph.size must be equalTo 287
      }
    }

  }
}
