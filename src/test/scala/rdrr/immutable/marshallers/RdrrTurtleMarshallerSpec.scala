package rdrr.immutable.marshallers

import org.scalatest.PrivateMethodTester
import org.specs2.mutable.Specification
import org.specs2.specification.Scope
import rdrr.immutable._

class RdrrTurtleMarshallerSpec extends Specification {


  "The RDRR Marshaller" should {

    "serialise resources to turtle" in new RdrrTurtleMarshallerScope {
      val resource = Resource("http://ontologies.com/ontology/resource")
      marshaller invokePrivate asTurtle(resource) must be equalTo "<http://ontologies.com/ontology/resource>"
    }

    "serialise predicates to turtle" in new RdrrTurtleMarshallerScope {
      val predicate = Resource("http://ontologies.com/ontology/Resource")
      marshaller invokePrivate asTurtle(predicate) must be equalTo "<http://ontologies.com/ontology/Resource>"
    }

    "serialise the RDF standard 'type' Resource to 'a'" in new RdrrTurtleMarshallerScope {
      val predicate = Resource("http://www.w3.org/1999/02/22-rdf-syntax-ns#type")
      marshaller invokePrivate asTurtle(predicate) must be equalTo "a"
    }

    "serialise standard string literals to turtle" in new RdrrTurtleMarshallerScope {
      val stringLiteral = StandardStringLiteral("JME")
      marshaller invokePrivate asTurtle(stringLiteral) must be equalTo "\"JME\""
    }

    "serialise nonstandard string literals to turtle" in new RdrrTurtleMarshallerScope {
      val stringLiteral = NonStandardStringLiteral("not normal", Resource("http://strings.co.uk/super"))
      marshaller invokePrivate asTurtle(stringLiteral) must be equalTo
        "\"not normal\"^^<http://strings.co.uk/super>"
    }

    "serialise language string literals to turtle" in new RdrrTurtleMarshallerScope {
      val stringLiteral = LanguageStringLiteral("chat", "fr")
      marshaller invokePrivate asTurtle(stringLiteral) must be equalTo "\"chat\"@fr"
    }

    "serialise boolean literals to turtle" in new RdrrTurtleMarshallerScope {
      val booleanLiteral = BooleanLiteral(true)
      marshaller invokePrivate asTurtle(booleanLiteral) must be equalTo "true^^<http://www.w3.org/TR/xmlschema-2/#boolean>"
    }

    "serialise integer literals to turtle" in new RdrrTurtleMarshallerScope {
      val integerLiteral = IntegerLiteral(8)
      marshaller invokePrivate asTurtle(integerLiteral) must be equalTo "8^^<http://www.w3.org/2001/XMLSchema#integer>"
    }

    "serialise decimal literals to turtle" in new RdrrTurtleMarshallerScope {
      val decimalLiteral = DecimalLiteral(3.14159)
      marshaller invokePrivate asTurtle(decimalLiteral) must be equalTo "3.14159^^<http://www.w3.org/2001/XMLSchema#decimal>"
    }

    "serialise blank nodes to turtle" in new RdrrTurtleMarshallerScope {
      val blankNode = BlankNode("label")
      marshaller invokePrivate asTurtle(blankNode) must be equalTo "_:label"
    }

    "correctly output the empty graph" in new RdrrTurtleMarshallerScope {
      marshaller.toTurtle(Graph.Empty) must be equalTo ""
    }

    "serialise a simple graph to Turtle" in new RdrrTurtleMarshallerScope {
      val graph = Graph(Triple(
        Resource("https://en.wikipedia.org/wiki/Justin_Bieber"),
        Resource("http://www.w3.org/1999/02/22-rdf-syntax-ns#type"),
        Resource("http://purl.org/ontology/mo/MusicArtist") ) )
      val expected = "<https://en.wikipedia.org/wiki/Justin_Bieber> a <http://purl.org/ontology/mo/MusicArtist> ."

      marshaller.toTurtle(graph) must beEqualTo (expected).ignoreSpace
    }

    "serialise to terse Turtle" in new RdrrTurtleMarshallerScope {
      val graph = Graph (

        Triple(Resource("https://en.wikipedia.org/wiki/Justin_Bieber"),
          Resource("http://www.w3.org/1999/02/22-rdf-syntax-ns#type"),
          Resource("http://purl.org/ontology/mo/MusicArtist") ),

        Triple(Resource("https://en.wikipedia.org/wiki/Justin_Bieber"),
          Resource("http://www.w3.org/1999/02/22-rdf-syntax-ns#type"),
          Resource("http://xmlns.com/foaf/0.1/Person") ),

        Triple(Resource("https://en.wikipedia.org/wiki/Justin_Bieber"),
          Resource("http://xmlns.com/foaf/0.1/name"),
          StandardStringLiteral("Justin Bieber") ) )

      val expected =
        """
          |<https://en.wikipedia.org/wiki/Justin_Bieber>
          |   a <http://purl.org/ontology/mo/MusicArtist>, <http://xmlns.com/foaf/0.1/Person> ;
          |   <http://xmlns.com/foaf/0.1/name> "Justin Bieber" .
        """.stripMargin

      marshaller.toTurtle(graph) must beEqualTo (expected).ignoreSpace
    }

    "correctly output labeled blank nodes" in new RdrrTurtleMarshallerScope {
      val graph = Graph(Triple(
        subject = BlankNode("some_num"),
        predicate = Resource("http://maths.org/greaterThan"),
        `object` = Resource("http://maths.org/Aleph0") ) )

      val expected = """_:some_num <http://maths.org/greaterThan> <http://maths.org/Aleph0> ."""

      marshaller.toTurtle(graph) must beEqualTo (expected).ignoreSpace
    }

  }

}

trait RdrrTurtleMarshallerScope extends Scope with PrivateMethodTester {
  val marshaller = RdrrTurtleMarshaller

  val asTurtle = PrivateMethod[String]('asTurtle)
}