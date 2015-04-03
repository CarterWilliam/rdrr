package jesc.immutable

import jesc.immutable.serialisers.{ParseException, TurtleSerialiser}
import org.scalatest.PrivateMethodTester
import org.specs2.mutable.Specification
import org.specs2.specification.Scope
import utilities.TestHelpers

class TurtleSerialiserSpec extends Specification with PrivateMethodTester {

  "The Turtle Serialiser" can {
    "serialise from triples with Resources from Turtle" in new TurtleSerialiserScope {
      val turtle = getResource("bieber-is-an-artist.ttl")
      val graph = serialiser.fromTurtle(turtle)
      graph.triples must have size 1
    }

//    "serialise from triples with resources and literals from Turtle" in new TurtleSerialiserScope {
//      val turtle = getResource("bieber.ttl")
//      val graph = serialiser.fromTurtle(turtle)
//      graph.triples must have size 3
//    }

    "create resources" in {
      "from URI turtle format" in new TurtleSerialiserScope {
        val resourceFromTurtle = PrivateMethod[Resource]('resourceFromTurtle)
        val resource = serialiser invokePrivate resourceFromTurtle("<http://purl.org/ontology/mo/MusicArtist>", Map.empty)
        resource must be equalTo Resource("http://purl.org/ontology/mo/MusicArtist")
      }
      "from prefix turtle format" in new TurtleSerialiserScope {
        val resourceFromTurtle = PrivateMethod[Resource]('resourceFromTurtle)
        val prefixes = Map("mo" -> "http://purl.org/ontology/mo/")
        val resource = serialiser invokePrivate resourceFromTurtle("mo:MusicArtist", prefixes)
        resource must be equalTo Resource("http://purl.org/ontology/mo/MusicArtist")
      }
      "throwing an exception if a prefixed resource is found with no matching prefix" in new TurtleSerialiserScope {
        val resourceFromTurtle = PrivateMethod[Resource]('resourceFromTurtle)
        serialiser invokePrivate resourceFromTurtle("mo:MusicArtist", Map.empty) must throwA[ParseException]
      }
      "throwing an exception if a resource is not in a recognised format" in new TurtleSerialiserScope {
        val resourceFromTurtle = PrivateMethod[Resource]('resourceFromTurtle)
        serialiser invokePrivate resourceFromTurtle("moMusicArtist", Map.empty) must throwA[ParseException]
      }
    }

    "create predicates" in {
      "from URI turtle format" in new TurtleSerialiserScope {
        val predicateFromTurtle = PrivateMethod[Predicate]('predicateFromTurtle)
        val predicate = serialiser invokePrivate predicateFromTurtle("<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>", Map.empty)
        predicate must be equalTo Predicate("http://www.w3.org/1999/02/22-rdf-syntax-ns#type")
      }
      "from prefix turtle format" in new TurtleSerialiserScope {
        val predicateFromTurtle = PrivateMethod[Predicate]('predicateFromTurtle)
        val prefixes = Map("rdfs" -> "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
        val predicate = serialiser invokePrivate predicateFromTurtle("rdfs:type", prefixes)
        predicate must be equalTo Predicate("http://www.w3.org/1999/02/22-rdf-syntax-ns#type")
      }
      "throwing an exception if a prefixed resource is found with no matching prefix" in new TurtleSerialiserScope {
        val predicateFromTurtle = PrivateMethod[Predicate]('predicateFromTurtle)
        serialiser invokePrivate predicateFromTurtle("mo:MusicArtist", Map.empty) must throwA[ParseException]
      }
      "from RDF standard 'a' meaning type" in new TurtleSerialiserScope {
        val predicateFromTurtle = PrivateMethod[Predicate]('predicateFromTurtle)
        val prefixes = Map("rdfs" -> "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
        val predicate = serialiser invokePrivate predicateFromTurtle("a", prefixes)
        predicate must be equalTo Predicate("http://www.w3.org/1999/02/22-rdf-syntax-ns#type")
      }
    }
  }
}

trait TurtleSerialiserScope extends Scope with TestHelpers {
  val serialiser = new TurtleSerialiser()
}
