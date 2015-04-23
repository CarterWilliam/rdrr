package rdrr.immutable

import rdrr.immutable.marshallers.JenaMarshaller
import rdrr.util.JavaHelpers


case class Graph(triples: Seq[Triple]) {

  lazy val subjects: Seq[Resource] = triples.map(_.subject).distinct

  def + (∆ : Triple): Graph = Graph(triples :+ ∆)

  def contains(∆ : Triple): Boolean = triples.contains(∆)

  def filter(constraint: Triple => Boolean) =
    Graph(triples.filter(constraint))

  def collect[T](partialFunction: PartialFunction[Triple, T]): Seq[T] =
    triples.collect(partialFunction)
}

object Graph extends JavaHelpers {

  def apply(firstTriple: Triple, moreTriples: Triple*): Graph = Graph(firstTriple +: moreTriples)
  val Empty = Graph(Stream.Empty)

  val marshaller = new JenaMarshaller
  def parse(turtle: String): Graph = marshaller.fromTurtle(turtle)
  def toTurtle(graph: Graph): String = marshaller.toTurtle(graph)
}
