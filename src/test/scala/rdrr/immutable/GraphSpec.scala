package rdrr.immutable

import org.specs2.mutable.Specification
import org.specs2.specification.Scope

class GraphSpec extends Specification {

  "A Graph" should {
    "have a list of subject, predicate object statements" in new GraphScope {
      val graph = Graph(JustinBieberIsAnArtist)
      graph must have size 1
    }
    "have a list of distinct subjects" in new GraphScope {
      val graph = Graph(JustinBieberIsAnArtist, JustinBieberHasAName)
      graph must have size 2
      graph.subjects must have size 1
    }
    "be able to be queried for specific triples" in new GraphScope {
      val graph = Graph(JustinBieberIsAnArtist)
      graph.contains(JustinBieber, a, Artist) must beTrue
      graph.contains(JustinBieberHasAName) must beFalse
    }

    "concatenation" in {
    
      "allow concatenation" in new GraphScope {
        val graphWithAllTriples = Graph(JustinBieberIsAnArtist, JustinBieberHasAName)
        Graph(JustinBieberIsAnArtist) ++ Graph(JustinBieberHasAName) must be equalTo graphWithAllTriples
      }
      
      "remove duplicate triples when concatenated with another graph" in new GraphScope {
        val graph = Graph(JustinBieberIsAnArtist) ++ Graph(JustinBieberIsAnArtist, JustinBieberHasAName)
        graph must have size 2
      }
    }
    
    "appending elements" in {
    
      "allow appending additional triples" in new GraphScope {
        Graph(JustinBieberIsAnArtist) :+ JustinBieberHasAName must be equalTo Graph(JustinBieberIsAnArtist, JustinBieberHasAName)
      }

      "allow prepending additional triples" in new GraphScope {
        JustinBieberHasAName +: Graph(JustinBieberIsAnArtist) must be equalTo Graph(JustinBieberHasAName, JustinBieberIsAnArtist)
      }

      "remove duplicate triples when appending/prepending" in new GraphScope {
        val graph = JustinBieberIsAnArtist +: Graph(JustinBieberIsAnArtist) :+ JustinBieberIsAnArtist
        graph must have size 1
      }

    }

    "filtering and transformation" in {

      "allow filtering of the triples by a predicate" in new GraphScope {
        val graph = Graph(JustinBieberIsAnArtist, JustinBieberHasAName, CatrionaKnowsJustin)
        val aboutPeopleThatCatrionaKnows = graph.filter { triple =>
          graph.contains(Catriona, knows, triple.subject)
        }
        aboutPeopleThatCatrionaKnows must be equalTo Graph(JustinBieberIsAnArtist, JustinBieberHasAName)
      }

      "allow filtering of the triples by a partial functions that transform the data" in new GraphScope {
        val graph = Graph(JustinBieberIsAnArtist, JustinBieberHasAName, CatrionaKnowsJustin)
        val TypePredicate = a

        val typesKnownByCatriona = graph.collect {
          case Triple(person, TypePredicate, personType) if graph.contains(Triple(Catriona, knows, person)) =>
            personType.as[Resource].uri.split("/").last
        }
        typesKnownByCatriona.head must be equalTo "MusicArtist"
      }

      "allow filtering and transforming of the triples by a partial function, removing duplicates" in new GraphScope {
        val graph = Graph(JustinBieberIsAnArtist, JustinBieberHasAName, CatrionaKnowsJustin)

        val transformed = graph.transform {
          case Triple(JustinBieber, p, o) if p != name => Triple(Catriona, p, o)
        }
        transformed must be equalTo Graph(Triple(Catriona, a, Artist))
      }

    }
  }
}

trait GraphScope extends Scope {
  val JustinBieber = Resource("https://en.wikipedia.org/wiki/Justin_Bieber")
  val a = Predicate("http://www.w3.org/1999/02/22-rdf-syntax-ns#type")
  val Artist = Resource("http://purl.org/ontology/mo/MusicArtist")
  val name = Predicate("http://xmlns.com/foaf/0.1/name")
  val Catriona = Resource("https://en.wikipedia.org/wiki/Catriona")
  val knows = Predicate("http://xmlns.com/foaf/spec/#term_knows")

  val JustinBieberIsAnArtist = Triple(JustinBieber, a, Artist)
  val JustinBieberHasAName = Triple(JustinBieber, name, StandardStringLiteral("Justin Bieber"))
  val CatrionaKnowsJustin = Triple(Catriona, knows, JustinBieber)
}
