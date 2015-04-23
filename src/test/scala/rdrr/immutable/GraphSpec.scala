package rdrr.immutable

import org.specs2.mutable.Specification
import org.specs2.specification.Scope

class GraphSpec extends Specification {

  "A Graph" should {
    "have a list of subject, predicate object statements" in new GraphScope {
      val graph = Graph(JustinBieberIsAnArtist)
      graph.triples must have size 1
    }
    "have a list of distinct subjects" in new GraphScope {
      val graph = Graph(JustinBieberIsAnArtist, JustinBieberHasAName)
      graph.triples must have size 2
      graph.subjects must have size 1
    }
    "be able to be queried for specific triples" in new GraphScope {
      val graph = Graph(JustinBieberIsAnArtist)
      graph.contains(Triple(JustinBieber, a, Artist)) must beTrue
      graph.contains(JustinBieberHasAName) must beFalse
    }
    "allow filtering" in {
      "generically" in new GraphScope {
        val graph = Graph(JustinBieberIsAnArtist, JustinBieberHasAName)
        graph.filter(_.predicate == a).triples must have size 1
      }
      "with partial functions that transform the data" in new GraphScope {
        val graph = Graph(JustinBieberIsAnArtist, JustinBieberHasAName, CatrionaKnowsJustin)
        val typesKnownByCatriona = graph.collect {
          case Triple(person, a, personType) if graph.contains(Triple(Catriona, knows, person)) => personType
        }
        typesKnownByCatriona.head must be equalTo Artist
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
  val JustinBieberHasAName = Triple(JustinBieber, name, StringLiteral("Justin Bieber"))
  val CatrionaKnowsJustin = Triple(Catriona, knows, JustinBieber)
}
