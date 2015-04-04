package rdrr.immutable.marshallers

import rdrr.immutable._
import org.scalatest.PrivateMethodTester
import org.specs2.mutable.Specification
import org.specs2.specification.Scope
import utilities.TestHelpers

class TurtleMarshallerSpec extends Specification with PrivateMethodTester {

  "The Turtle Marshaller" can {
    "marshal from" in {
      "triples with Resources from Turtle" in new TurtleMarshallerScope {
        val turtle = getResource("bieber-is-an-artist.ttl")
        val graph = marshaller.fromTurtle(turtle)
        graph.triples must have size 1
      }

      "triples with resources and literals from Turtle" in new TurtleMarshallerScope {
        val turtle = getResource("bieber.ttl")
        val graph = marshaller.fromTurtle(turtle)
        graph.triples must have size 3
      }
    }

    "marshall to Turtle" in new TurtleMarshallerScope {
      val graph = Graph.Empty + Triple(
        Resource("https://en.wikipedia.org/wiki/Justin_Bieber"),
        Predicate("http://www.w3.org/1999/02/22-rdf-syntax-ns#type"),
        Resource("http://purl.org/ontology/mo/MusicArtist") )

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
        literal must be equalTo StringLiteral("Justin Bieber")
      }
      "from a turtle string with a language" in new CreateLiteralsScope {
        val literal = marshaller invokePrivate nodeFromTurtle("\"Justine Biebere\"@fr", Map.empty)
        literal must be equalTo LanguageStringLiteral("Justine Biebere", "fr")
      }
      "from a turtle string with a custom IRI" in new CreateLiteralsScope {
        val literal = marshaller invokePrivate nodeFromTurtle("\"Justin Bieber\"^^<http://www.crazyencodings.co.uk/encoding>", Map.empty)
        literal must be equalTo NonStandardStringLiteral("Justin Bieber", "http://www.crazyencodings.co.uk/encoding")
      }
    }

  }
}

trait TurtleMarshallerScope extends Scope with TestHelpers {
  val marshaller = new TurtleMarshaller()
}

trait IriExtractScope extends Scope with PrivateMethodTester {
  val marshaller = new TurtleMarshaller()
  val iriFromTurtleRepresentation = PrivateMethod[String]('iriFromTurtleRepresentation)
}

trait CreateLiteralsScope extends Scope with PrivateMethodTester {
  val marshaller = new TurtleMarshaller()
  val nodeFromTurtle = PrivateMethod[Node]('nodeFromTurtle)
}

trait SplitResourceStringScope extends Scope with PrivateMethodTester {
  val marshaller = new TurtleMarshaller()
  type Tup2 = (String, String)
  val splitResourceString = PrivateMethod[Tup2]('splitResourceString)
}