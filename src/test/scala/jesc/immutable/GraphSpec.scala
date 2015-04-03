package jesc.immutable

import org.specs2.mutable.Specification
import org.specs2.specification.Scope

class GraphSpec extends Specification {

  "A Graph" should {
    "have a list of subject, predicate object statements" in new GraphScope {
      val graph = Graph(Stream(
        JustinBieberIsAnArtist ) )
      graph.triples must have size 1
    }
    "have a list of distinct subjects" in new GraphScope {
      val graph = Graph(Stream(
        JustinBieberIsAnArtist,
        JustinBieberHasAName ) )
      graph.triples must have size 2
      graph.subjects must have size 1
    }
  }
}

trait GraphScope extends Scope {
  val JustinBieber = Resource("https://en.wikipedia.org/wiki/Justin_Bieber")
  val a = Predicate("http://www.w3.org/1999/02/22-rdf-syntax-ns#type")
  val Artist = Resource("http://purl.org/ontology/mo/MusicArtist")
  val name = Predicate("http://xmlns.com/foaf/0.1/name")

  val JustinBieberIsAnArtist = Triple(JustinBieber, a, Artist)
  val JustinBieberHasAName = Triple(JustinBieber, name, StringLiteral("Justin Bieber"))
}
