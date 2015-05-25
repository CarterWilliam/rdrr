package rdrr.immutable.marshallers

import org.specs2.mutable.Specification
import org.specs2.specification.Scope
import rdrr.immutable.Graph
import utilities.TestHelpers

class TurtleMarshallerSpec extends Specification {

  case class MarshallerImplementation(name: String, marshaller: TurtleMarshaller, unmarshaller: TurtleUnmarshaller)

  val rdrrImplementation = MarshallerImplementation("RDRR", RdrrTurtleMarshaller, RdrrTurtleUnmarshaller)
  val jenaImplementation = MarshallerImplementation("Jena", JenaTurtleMarshaller, JenaTurtleUnmarshaller)

  Seq(jenaImplementation, rdrrImplementation).foreach { implementation =>

    val marshaller = implementation.marshaller

    s"The ${implementation.name} turtle marshaller" should {

      "correctly output the empty graph" in new Scope with TestHelpers {
        marshaller.toTurtle(Graph.Empty) must be equalTo ""
      }
    }


    val unmarshaller = implementation.unmarshaller

    s"The ${implementation.name} turtle unmarshaller" should {

      "be able to handle a large turtle file" in new Scope with TestHelpers {
        val turtle = getResource("bbc.ttl")
        val graph = unmarshaller.fromTurtle(turtle)
        graph.size must be equalTo 287
      }

      "be able to handle base prefixes" in new Scope with TestHelpers {
        val turtle = getResource("base-and-empty-prefixes.ttl")
        val graph = unmarshaller.fromTurtle(turtle)
        graph must have size 2
        graph.subjects must have size 1
      }
    }

  }
}
