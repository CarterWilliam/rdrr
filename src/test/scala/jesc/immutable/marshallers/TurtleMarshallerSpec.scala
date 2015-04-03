package jesc.immutable.marshallers

import jesc.immutable._
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

      val expected = "<https://en.wikipedia.org/wiki/Justin_Bieber a <http://purl.org/ontology/mo/MusicArtist> ."
      marshaller.toTurtle(graph) must beEqualTo (expected).ignoreSpace
    }

    "create resources" in {
      "from URI turtle format" in new TurtleMarshallerScope {
        val resourceFromTurtle = PrivateMethod[Resource]('resourceFromTurtle)
        val resource = marshaller invokePrivate resourceFromTurtle("<http://purl.org/ontology/mo/MusicArtist>", Map.empty)
        resource must be equalTo Resource("http://purl.org/ontology/mo/MusicArtist")
      }
      "from prefix turtle format" in new TurtleMarshallerScope {
        val resourceFromTurtle = PrivateMethod[Resource]('resourceFromTurtle)
        val prefixes = Map("mo" -> "http://purl.org/ontology/mo/")
        val resource = marshaller invokePrivate resourceFromTurtle("mo:MusicArtist", prefixes)
        resource must be equalTo Resource("http://purl.org/ontology/mo/MusicArtist")
      }
      "throwing an exception if a prefixed resource is found with no matching prefix" in new TurtleMarshallerScope {
        val resourceFromTurtle = PrivateMethod[Resource]('resourceFromTurtle)
        marshaller invokePrivate resourceFromTurtle("mo:MusicArtist", Map.empty) must throwA[ParseException]
      }
      "throwing an exception if a resource is not in a recognised format" in new TurtleMarshallerScope {
        val resourceFromTurtle = PrivateMethod[Resource]('resourceFromTurtle)
        marshaller invokePrivate resourceFromTurtle("moMusicArtist", Map.empty) must throwA[ParseException]
      }
    }

    "create predicates" in {
      "from URI turtle format" in new TurtleMarshallerScope {
        val predicateFromTurtle = PrivateMethod[Predicate]('predicateFromTurtle)
        val predicate = marshaller invokePrivate predicateFromTurtle("<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>", Map.empty)
        predicate must be equalTo Predicate("http://www.w3.org/1999/02/22-rdf-syntax-ns#type")
      }
      "from prefix turtle format" in new TurtleMarshallerScope {
        val predicateFromTurtle = PrivateMethod[Predicate]('predicateFromTurtle)
        val prefixes = Map("rdfs" -> "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
        val predicate = marshaller invokePrivate predicateFromTurtle("rdfs:type", prefixes)
        predicate must be equalTo Predicate("http://www.w3.org/1999/02/22-rdf-syntax-ns#type")
      }
      "throwing an exception if a prefixed resource is found with no matching prefix" in new TurtleMarshallerScope {
        val predicateFromTurtle = PrivateMethod[Predicate]('predicateFromTurtle)
        marshaller invokePrivate predicateFromTurtle("mo:MusicArtist", Map.empty) must throwA[ParseException]
      }
      "from RDF standard 'a' meaning type" in new TurtleMarshallerScope {
        val predicateFromTurtle = PrivateMethod[Predicate]('predicateFromTurtle)
        val prefixes = Map("rdfs" -> "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
        val predicate = marshaller invokePrivate predicateFromTurtle("a", prefixes)
        predicate must be equalTo Predicate("http://www.w3.org/1999/02/22-rdf-syntax-ns#type")
      }
    }

    "create nodes" in {
      "from a turtle string without a language" in new TurtleMarshallerScope {
        val nodeFromTurtle = PrivateMethod[Node]('nodeFromTurtle)
        val literal = marshaller invokePrivate nodeFromTurtle("\"Justin Bieber\"", Map.empty)
        literal must be equalTo StringLiteral("Justin Bieber", "en")
      }
      "from a turtle string with a language" in new TurtleMarshallerScope {
        val nodeFromTurtle = PrivateMethod[Node]('nodeFromTurtle)
        val literal = marshaller invokePrivate nodeFromTurtle("\"Justine Biebere\"@fr", Map.empty)
        literal must be equalTo StringLiteral("Justine Biebere", "fr")
      }
    }

    "extract" in {
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
  }
}

trait TurtleMarshallerScope extends Scope with TestHelpers {
  val marshaller = new TurtleMarshaller()
}

trait SplitResourceStringScope extends Scope with PrivateMethodTester {
  val marshaller = new TurtleMarshaller()

  type Tup2 = (String, String)
  val splitResourceString = PrivateMethod[Tup2]('splitResourceString)
}