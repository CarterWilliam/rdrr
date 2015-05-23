package rdrr.immutable

import org.specs2.mutable.Specification
import org.specs2.specification.Scope
import rdrr.immutable.marshallers.Prefix

class RdfGraphSpec extends Specification {

  "An RdfGraph" should {

    "have a list of subject, predicate object statements" in new GraphScope {
      val graph = RdfGraph(JustinBieberIsAnArtist)
      graph must have size 1
    }

    "have a list of distinct subjects" in new GraphScope {
      val graph = RdfGraph(JustinBieberIsAnArtist, JustinBieberHasAName)
      graph must have size 2
      graph.subjects must have size 1
    }

    "be able to be queried for specific triples" in new GraphScope {
      val graph = RdfGraph(JustinBieberIsAnArtist)
      graph.contains(JustinBieber, a, Artist) must beTrue
      graph.contains(JustinBieberHasAName) must beFalse
    }

    "concatenation" in {

      "allow concatenation" in new GraphScope {
        val graphWithAllTriples = RdfGraph(JustinBieberIsAnArtist, JustinBieberHasAName)
        RdfGraph(JustinBieberIsAnArtist) ++ RdfGraph(JustinBieberHasAName) must be equalTo graphWithAllTriples
      }

      "remove duplicate triples when concatenated with another graph" in new GraphScope {
        val graph = RdfGraph(JustinBieberIsAnArtist) ++ RdfGraph(JustinBieberIsAnArtist, JustinBieberHasAName)
        graph.triples must have size 2
      }

      "concatenate the list of prefixes, removing duplicates if necessary" in new GraphScope {
        val graph1 = RdfGraph (
          triples = Seq(JustinBieberIsAnArtist),
          prefixes = Seq(wikipediaPrefix, musicOntologyPrefix) )
        val graph2 = RdfGraph (
          triples = Seq(JustinKnowsCatriona),
          prefixes = Seq(wikipediaPrefix, foafPrefix) )

        (graph1 ++ graph2).prefixes must be equalTo Seq(wikipediaPrefix, musicOntologyPrefix, foafPrefix)
      }
    }

    "appending elements" in {

      "allow appending additional triples" in new GraphScope {
        RdfGraph(JustinBieberIsAnArtist) :+ JustinBieberHasAName must be equalTo
          RdfGraph(JustinBieberIsAnArtist, JustinBieberHasAName)
      }

      "allow prepending additional triples" in new GraphScope {
        JustinBieberHasAName +: RdfGraph(JustinBieberIsAnArtist) must be equalTo
          RdfGraph(JustinBieberHasAName, JustinBieberIsAnArtist)
      }

      "remove duplicate triples when appending/prepending" in new GraphScope {
        val graph = JustinBieberIsAnArtist +: RdfGraph(JustinBieberIsAnArtist) :+ JustinBieberIsAnArtist
        graph must have size 1
      }

      "allow appending additional prefixes" in new GraphScope {
        val graph = RdfGraph(prefixes = Seq(musicOntologyPrefix)) + foafPrefix
        graph.prefixes must be equalTo Seq(musicOntologyPrefix, foafPrefix)
      }
    }

    "filtering and transformation" in {
      
      "allow filtering of the triples by a predicate" in new GraphScope {
        val graph = RdfGraph(JustinBieberIsAnArtist, JustinBieberHasAName, CatrionaKnowsJustin)
        val knownByCatriona = graph.filter { triple =>
          graph.contains(Catriona, knows, triple.subject)
        }
        knownByCatriona.head must be equalTo JustinBieberIsAnArtist
      }

      "allow collecting and transforming of the triples" in new GraphScope {
        val graph = RdfGraph(JustinBieberIsAnArtist, JustinBieberHasAName, CatrionaKnowsJustin)
        val RdfsType = a
        val typesKnownByCatriona = graph.collect {
          case Triple(person, RdfsType, personType) if graph.contains(Triple(Catriona, knows, person)) =>
            personType.as[Resource].uri.split("/").last
        }
        typesKnownByCatriona.head must be equalTo "MusicArtist"
      }

      "allow filtering and transforming of the triples by a partial function, retaining graph prefixes and removing duplicates" in new GraphScope {
        val graph = RdfGraph(
          triples = Seq(JustinBieberIsAnArtist, JustinBieberHasAName, CatrionaKnowsJustin),
          prefixes = Seq(foafPrefix))

        val transformed = graph.transform {
          case Triple(JustinBieber, p, o) => JustinBieberIsAnArtist
          case CatrionaKnowsJustin => CatrionaKnowsJustin
        }

        transformed must have size 2
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
  val JustinKnowsCatriona = Triple(JustinBieber, knows, Catriona)

  val beiberGraph = RdfGraph(JustinBieberIsAnArtist, JustinBieberHasAName)

  val wikipediaPrefix = Prefix("wiki", "https://en.wikipedia.org/wiki/")
  val musicOntologyPrefix = Prefix("mo", "http://purl.org/ontology/mo/")
  val foafPrefix = Prefix("foaf", "http://xmlns.com/foaf/0.1/")

  val beiberGraphWithPrefixes = RdfGraph (
    triples = Seq(JustinBieberIsAnArtist, JustinBieberHasAName),
    prefixes = Seq(wikipediaPrefix, musicOntologyPrefix, foafPrefix) )

}
