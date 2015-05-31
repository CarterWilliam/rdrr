package rdrr.immutable.marshallers

import org.specs2.mutable.Specification
import org.specs2.specification.Scope
import rdrr.immutable._
import utilities.{PredictableRandomStrings, TestHelpers}

class RdrrTurtleUnmarshallerAcceptanceSpec extends Specification {

  "The RdrrTurtleUnmarshaller can marshal from" in {

    "a turtle graph with prefixes" in new RdrrTurtleUnmarshallerAcceptanceScope {
      val turtle = """
                     |@base <https://en.wikipedia.org/wiki/> .
                     |@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
                     |@prefix : <http://purl.org/ontology/mo/> .
                     |<Justin_Bieber> rdf:type :MusicArtist .
                   """.stripMargin
      val graph = unmarshaller.fromTurtle(turtle)

      graph must be equalTo Graph(
        Triple(Resource("https://en.wikipedia.org/wiki/Justin_Bieber"),
          Predicate("http://www.w3.org/1999/02/22-rdf-syntax-ns#type"),
          Resource("http://purl.org/ontology/mo/MusicArtist")) )
    }

    "a turtle graph with prefixes defined using SPARQL syntax" in new RdrrTurtleUnmarshallerAcceptanceScope {
      val turtle = """
                     |BASE <https://en.wikipedia.org/wiki/>
                     |PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
                     |PREFIX : <http://purl.org/ontology/mo/>
                     |<Justin_Bieber> rdf:type :MusicArtist .
                   """.stripMargin
      val graph = unmarshaller.fromTurtle(turtle)

      graph must be equalTo Graph(
        Triple(Resource("https://en.wikipedia.org/wiki/Justin_Bieber"),
          Predicate("http://www.w3.org/1999/02/22-rdf-syntax-ns#type"),
          Resource("http://purl.org/ontology/mo/MusicArtist")) )
    }

    "a turtle graph with literals" in new RdrrTurtleUnmarshallerAcceptanceScope {
      val literalGraph = """
          |@prefix mo: <http://purl.org/ontology/mo/> .
          |<https://en.wikipedia.org/wiki/Justin_Bieber>
          |    a mo:SoloMusicArtist ;
          |    <http://xmlns.com/foaf/0.1/name> "Justin Bieber"@en .
        """.stripMargin
      val graph = unmarshaller.fromTurtle(literalGraph)

      graph must containTheSameElementsAs{
        Graph(
          Triple(Resource("https://en.wikipedia.org/wiki/Justin_Bieber"),
            Predicate("http://www.w3.org/1999/02/22-rdf-syntax-ns#type"),
            Resource("http://purl.org/ontology/mo/SoloMusicArtist")),
          Triple(Resource("https://en.wikipedia.org/wiki/Justin_Bieber"),
            Predicate("http://xmlns.com/foaf/0.1/name"), LanguageStringLiteral("Justin Bieber", "en"))
        )
      }
    }

    "a turtle graph with labeled blank nodes" in new RdrrTurtleUnmarshallerAcceptanceScope {
      val blankNodeTurtle = """
          |@prefix foaf: <http://xmlns.com/foaf/0.1/> .
          |_:alice foaf:knows _:bob .
          |_:bob foaf:knows _:alice .
        """.stripMargin
      val graph = unmarshaller.fromTurtle(blankNodeTurtle)

      graph must containTheSameElementsAs {
        Graph (
          Triple(BlankNode("alice"), Predicate("http://xmlns.com/foaf/0.1/knows"), BlankNode("bob")),
          Triple(BlankNode("bob"), Predicate("http://xmlns.com/foaf/0.1/knows"), BlankNode("alice"))
        )
      }
    }

    "a turtle graph with unlabeled blank nodes" in new RdrrTurtleUnmarshallerAcceptanceScope {
      val blankNodeTurtle =
        """[] <http://xmlns.com/foaf/0.1/knows> [] . """
      val graph = unmarshaller.fromTurtle(blankNodeTurtle)
      graph must be equalTo Graph {
        Triple(BlankNode("blank-1"), Predicate("http://xmlns.com/foaf/0.1/knows"), BlankNode("blank-2"))
      }
    }

    "a turtle graph with unlabeled blank nodes containing nested triples" in {

      "in the subject" in new RdrrTurtleUnmarshallerAcceptanceScope {
        val bobKnowsSomeone = """
                                |@prefix foaf: <http://xmlns.com/foaf/0.1/> .
                                |  [ foaf:name "Bob" ] foaf:knows [] .
                              """.stripMargin
        val graph = unmarshaller.fromTurtle(bobKnowsSomeone)

        graph must be equalTo Graph(
          Triple(BlankNode("blank-1"), Predicate("http://xmlns.com/foaf/0.1/knows"), BlankNode("blank-2")),
          Triple(BlankNode("blank-1"), Predicate("http://xmlns.com/foaf/0.1/name"), StandardStringLiteral("Bob"))
        )
      }

      "in the object" in new RdrrTurtleUnmarshallerAcceptanceScope {
        val someoneKnowsBob = """ # http://www.w3.org/TR/turtle/#BNodes - Example 15
                                |@prefix foaf: <http://xmlns.com/foaf/0.1/> .
                                |  [] foaf:knows [ foaf:name "Bob" ] .
                              """.stripMargin
        val graph = unmarshaller.fromTurtle(someoneKnowsBob)

        graph must be equalTo Graph(
          Triple(BlankNode("blank-1"), Predicate("http://xmlns.com/foaf/0.1/knows"), BlankNode("blank-2")),
          Triple(BlankNode("blank-2"), Predicate("http://xmlns.com/foaf/0.1/name"), StandardStringLiteral("Bob"))
        )
      }

      "within nested triples" in new RdrrTurtleUnmarshallerAcceptanceScope {
        val doublyNestedBlankNodes = getResource("complex-anonymous-nested-blank-nodes.ttl")
        val graph = unmarshaller.fromTurtle(doublyNestedBlankNodes)
        graph must containTheSameElementsAs {
          Graph(
            Triple(BlankNode("blank-1"), Predicate("http://xmlns.com/foaf/0.1/name"), StandardStringLiteral("Alice")),
            Triple(BlankNode("blank-1"), Predicate("http://xmlns.com/foaf/0.1/knows"), BlankNode("blank-2")),
            Triple(BlankNode("blank-2"), Predicate("http://xmlns.com/foaf/0.1/name"), StandardStringLiteral("Bob")),
            Triple(BlankNode("blank-2"), Predicate("http://xmlns.com/foaf/0.1/mbox"), Resource("bob:example.com")),
            Triple(BlankNode("blank-2"), Predicate("http://xmlns.com/foaf/0.1/knows"), BlankNode("blank-3")),
            Triple(BlankNode("blank-3"), Predicate("http://xmlns.com/foaf/0.1/name"), StandardStringLiteral("Eve"))
          )
        }
      }
    }

    "a turtle graph with the empty collection" in new RdrrTurtleUnmarshallerAcceptanceScope {
      val someoneKnowsZero =
        """
          |@prefix foaf: <http://xmlns.com/foaf/0.1/> .
          |  [] foaf:knows () .
        """.stripMargin

      val graph = unmarshaller.fromTurtle(someoneKnowsZero)
      graph must be equalTo Graph (
        Triple(BlankNode("blank-1"), Predicate("http://xmlns.com/foaf/0.1/knows"), Resource("http://www.w3.org/1999/02/22-rdf-syntax-ns#nil"))
      )
    }

    "a turtle graph with a collection of cardinality > 0" in new RdrrTurtleUnmarshallerAcceptanceScope {
      val someoneKnowsZero =
        """
          |@prefix : <http://example.org/stuff/1.0/> .
          |  :a :b ( "apple" "banana" ) .
        """.stripMargin

      val graph = unmarshaller.fromTurtle(someoneKnowsZero)
      graph must be equalTo Graph (
        Triple(Resource("http://example.org/stuff/1.0/a"), Predicate("http://example.org/stuff/1.0/b"), BlankNode("blank-1")),
        Triple(BlankNode("blank-1"), Predicate("http://www.w3.org/1999/02/22-rdf-syntax-ns#first"), StandardStringLiteral("apple")),
        Triple(BlankNode("blank-1"), Predicate("http://www.w3.org/1999/02/22-rdf-syntax-ns#rest"), BlankNode("blank-2")),
        Triple(BlankNode("blank-2"), Predicate("http://www.w3.org/1999/02/22-rdf-syntax-ns#first"), StandardStringLiteral("banana")),
        Triple(BlankNode("blank-2"), Predicate("http://www.w3.org/1999/02/22-rdf-syntax-ns#rest"), Resource("http://www.w3.org/1999/02/22-rdf-syntax-ns#nil"))
      )
    }

    "a turtle graph with nested collections" in new RdrrTurtleUnmarshallerAcceptanceScope {
      val someoneKnowsZero =
        """
          |@prefix : <http://example.org/stuff/1.0/> .
          |  :a :b ( "apple" ( "banana" "clementine" ) ) .
        """.stripMargin

      val graph = unmarshaller.fromTurtle(someoneKnowsZero)
      graph must containTheSameElementsAs {
        Graph(
          Triple(Resource("http://example.org/stuff/1.0/a"), Predicate("http://example.org/stuff/1.0/b"), BlankNode("blank-1")),
          Triple(BlankNode("blank-1"), Predicate("http://www.w3.org/1999/02/22-rdf-syntax-ns#first"), StandardStringLiteral("apple")),
          Triple(BlankNode("blank-1"), Predicate("http://www.w3.org/1999/02/22-rdf-syntax-ns#rest"), BlankNode("blank-2")),
          Triple(BlankNode("blank-2"), Predicate("http://www.w3.org/1999/02/22-rdf-syntax-ns#first"), BlankNode("blank-3")),
          Triple(BlankNode("blank-2"), Predicate("http://www.w3.org/1999/02/22-rdf-syntax-ns#rest"), Resource("http://www.w3.org/1999/02/22-rdf-syntax-ns#nil")),

          Triple(BlankNode("blank-3"), Predicate("http://www.w3.org/1999/02/22-rdf-syntax-ns#first"), StandardStringLiteral("banana")),
          Triple(BlankNode("blank-3"), Predicate("http://www.w3.org/1999/02/22-rdf-syntax-ns#rest"), BlankNode("blank-4")),
          Triple(BlankNode("blank-4"), Predicate("http://www.w3.org/1999/02/22-rdf-syntax-ns#first"), StandardStringLiteral("clementine")),
          Triple(BlankNode("blank-4"), Predicate("http://www.w3.org/1999/02/22-rdf-syntax-ns#rest"), Resource("http://www.w3.org/1999/02/22-rdf-syntax-ns#nil"))
        )
      }
    }

    "a turtle graph with nested blank nodes in collections" in new RdrrTurtleUnmarshallerAcceptanceScope {
      val someoneKnowsZero =
        """
          |PREFIX : <http://example.org/stuff/1.0/>
          |(1 [:p :q] ( 2 ) ) :p2 :q2 .
        """.stripMargin

      val graph = unmarshaller.fromTurtle(someoneKnowsZero)
      graph must containTheSameElementsAs {
        Graph(
          Triple(BlankNode("blank-1"), Predicate("http://www.w3.org/1999/02/22-rdf-syntax-ns#first"), IntegerLiteral(1)),
          Triple(BlankNode("blank-1"), Predicate("http://www.w3.org/1999/02/22-rdf-syntax-ns#rest"), BlankNode("blank-2")),

          Triple(BlankNode("blank-2"), Predicate("http://www.w3.org/1999/02/22-rdf-syntax-ns#first"), BlankNode("blank-3")),
          Triple(BlankNode("blank-2"), Predicate("http://www.w3.org/1999/02/22-rdf-syntax-ns#rest"), BlankNode("blank-4")),

          Triple(BlankNode("blank-3"), Predicate("http://example.org/stuff/1.0/p"), Resource("http://example.org/stuff/1.0/q")),

          Triple(BlankNode("blank-4"), Predicate("http://www.w3.org/1999/02/22-rdf-syntax-ns#first"), BlankNode("blank-5")),
          Triple(BlankNode("blank-4"), Predicate("http://www.w3.org/1999/02/22-rdf-syntax-ns#rest"), Resource("http://www.w3.org/1999/02/22-rdf-syntax-ns#nil")),

          Triple(BlankNode("blank-5"), Predicate("http://www.w3.org/1999/02/22-rdf-syntax-ns#first"), IntegerLiteral(2)),
          Triple(BlankNode("blank-5"), Predicate("http://www.w3.org/1999/02/22-rdf-syntax-ns#rest"), Resource("http://www.w3.org/1999/02/22-rdf-syntax-ns#nil")),

          Triple(BlankNode("blank-1"), Predicate("http://example.org/stuff/1.0/p2"), Resource("http://example.org/stuff/1.0/q2"))
        )
      }
    }

    "a large turtle file" in new RdrrTurtleUnmarshallerAcceptanceScope {
      val turtle = getResource("bbc.ttl")
      val graph = unmarshaller.fromTurtle(turtle)
      graph.size must be equalTo 287
    }


  }

}

trait RdrrTurtleUnmarshallerAcceptanceScope extends Scope with TestHelpers {
  object unmarshaller extends RdrrTurtleUnmarshaller with PredictableRandomStrings
}
