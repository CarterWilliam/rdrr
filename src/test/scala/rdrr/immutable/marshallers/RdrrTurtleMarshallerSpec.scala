package rdrr.immutable.marshallers

import rdrr.immutable._
import org.scalatest.PrivateMethodTester
import org.specs2.mutable.Specification
import org.specs2.specification.Scope
import utilities.TestHelpers

class RdrrTurtleMarshallerSpec extends Specification with PrivateMethodTester {

  "The Turtle Marshaller" can {
    "marshal from" in {
      "triples with Resources from Turtle" in new RdrrTurtleMarshallerScope {
        val turtle = getResource("bieber-is-an-artist.ttl")
        val graph = marshaller.fromTurtle(turtle)
        graph must have size 1
      }

      "triples with resources and literals from Turtle" in new RdrrTurtleMarshallerScope {
        val turtle = getResource("bieber.ttl")
        val graph = marshaller.fromTurtle(turtle)
        graph must have size 3
      }
    }

    "marshall to Turtle" in new RdrrTurtleMarshallerScope {
      val graph = Graph(Triple(
        Resource("https://en.wikipedia.org/wiki/Justin_Bieber"),
        Predicate("http://www.w3.org/1999/02/22-rdf-syntax-ns#type"),
        Resource("http://purl.org/ontology/mo/MusicArtist") ) )

      val expected = "<https://en.wikipedia.org/wiki/Justin_Bieber> a <http://purl.org/ontology/mo/MusicArtist> ."
      marshaller.toTurtle(graph) must beEqualTo (expected).ignoreSpace
    }

    "extract inline resources" in {
      "the first resource from a string of resources" in new SplitResourceStringScope {
        val resourcesString = "wiki:Justin_Bieber a mo:Artist ."
        val splitResources = marshaller invokePrivate splitResourceString(resourcesString)
        splitResources must beEqualTo("wiki:Justin_Bieber", "a mo:Artist .")
      }
      "the first string literal from a string of resources" in new SplitResourceStringScope {
        val resourcesString = "\"Justin Bieber\" ."
        val splitResources = marshaller invokePrivate splitResourceString(resourcesString)
        splitResources must beEqualTo("\"Justin Bieber\"", ".")
      }
      "the first string literal with language from a string of resources" in new SplitResourceStringScope {
        val resourcesString = "\"Justin Bieber\"@en ;"
        val splitResources = marshaller invokePrivate splitResourceString(resourcesString)
        splitResources must beEqualTo("\"Justin Bieber\"@en", ";")
      }
      "the first string literal with custom datatype iri" in new SplitResourceStringScope {
        val resourcesString = "\"Justin Bieber\"^^xsd:string ,"
        val splitResources = marshaller invokePrivate splitResourceString(resourcesString)
        splitResources must beEqualTo("\"Justin Bieber\"^^xsd:string", ",")
      }
    }


    "extract uris" in {
      "from a turtle resource in IRI format" in new IriExtractScope {
        val typeIri = marshaller invokePrivate iriFromTurtleRepresentation("<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>", Map.empty)
        typeIri must be equalTo "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"
      }
      "from a turtle resource in prefix:name format" in new IriExtractScope {
        val prefixes = Map("mo" -> "http://purl.org/ontology/mo/")
        val musicArtistIri = marshaller invokePrivate iriFromTurtleRepresentation("mo:MusicArtist", prefixes)
        musicArtistIri must be equalTo "http://purl.org/ontology/mo/MusicArtist"
      }
      "from the RDF standard abbrieviation'a'" in new IriExtractScope {
        val typeIri = marshaller invokePrivate iriFromTurtleRepresentation("a", Map.empty)
        typeIri must be equalTo "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"
      }
      "throwing an exception if a prefixed resource has no matching prefix" in new IriExtractScope {
        marshaller invokePrivate iriFromTurtleRepresentation("mo:MusicArtist", Map.empty) must throwA[ParseException]
      }
      "throwing an exception if a resource is not in a recognised format" in new IriExtractScope {
        marshaller invokePrivate iriFromTurtleRepresentation("moMusicArtist", Map.empty) must throwA[ParseException]
      }
    }

    "create literals" in {
      "from a turtle string without a language" in new CreateLiteralsScope {
        val literal = marshaller invokePrivate nodeFromTurtle("\"Justin Bieber\"", Map.empty)
        literal must be equalTo StandardStringLiteral("Justin Bieber")
      }
      "from a turtle string with a language" in new CreateLiteralsScope {
        val literal = marshaller invokePrivate nodeFromTurtle("\"Justine Biebere\"@fr", Map.empty)
        literal must be equalTo LanguageStringLiteral("Justine Biebere", "fr")
      }
      "from a turtle string with a custom IRI" in new CreateLiteralsScope {
        val literal = marshaller invokePrivate nodeFromTurtle("\"Justin Bieber\"^^<http://www.crazyencodings.co.uk/encoding>", Map.empty)
        literal must be equalTo NonStandardStringLiteral("Justin Bieber", "http://www.crazyencodings.co.uk/encoding")
      }
      "from a turtle boolean" in new CreateLiteralsScope {
        val literal = marshaller invokePrivate nodeFromTurtle("false", Map.empty)
        literal must be equalTo BooleanLiteral(false)
      }
      "from a turtle integer" in new CreateLiteralsScope {
        val literal = marshaller invokePrivate nodeFromTurtle("684", Map.empty)
        literal must be equalTo IntegerLiteral(684)
      }
      "from an explicitly positive turtle integer" in new CreateLiteralsScope {
        val literal = marshaller invokePrivate nodeFromTurtle("+684", Map.empty)
        literal must be equalTo IntegerLiteral(684)
      }
      "from a negative turtle integer" in new CreateLiteralsScope {
        val literal = marshaller invokePrivate nodeFromTurtle("-684", Map.empty)
        literal must be equalTo IntegerLiteral(-684)
      }
      "from a turtle decimal" in new CreateLiteralsScope {
        val literal = marshaller invokePrivate nodeFromTurtle("6.84", Map.empty)
        literal must be equalTo DecimalLiteral(6.84)
      }
      "from a negative turtle decimal" in new CreateLiteralsScope {
        val literal = marshaller invokePrivate nodeFromTurtle("-6.84", Map.empty)
        literal must be equalTo DecimalLiteral(-6.84)
      }
    }

  }
}

trait RdrrTurtleMarshallerScope extends Scope with TestHelpers {
  val marshaller = new RdrrTurtleMarshaller()
}

trait IriExtractScope extends RdrrTurtleMarshallerScope with PrivateMethodTester {
  val iriFromTurtleRepresentation = PrivateMethod[String]('iriFromTurtleRepresentation)
}

trait CreateLiteralsScope extends RdrrTurtleMarshallerScope with PrivateMethodTester {
  val nodeFromTurtle = PrivateMethod[Node]('nodeFromTurtle)
}

trait SplitResourceStringScope extends RdrrTurtleMarshallerScope with PrivateMethodTester {
  type Tup2 = (String, String)
  val splitResourceString = PrivateMethod[Tup2]('splitResourceString)
}