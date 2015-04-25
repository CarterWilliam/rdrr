package rdrr.immutable

import rdrr.immutable.marshallers.{TurtleMarshaller, JenaTurtleMarshaller}

case class Triple(subject: Resource, predicate: Predicate, `object`: Node)

class Graph(triples: Seq[Triple]) extends Seq[Triple] {

  lazy val subjects: Seq[Resource] = triples.map(_.subject).distinct

  def + (∆ : Triple): Graph = Graph(triples :+ ∆)

  def contains(subject: Resource, predicate: Predicate, `object`: Node): Boolean =
    triples.contains(Triple(subject, predicate, `object`))

  def filter(subject: Option[Resource] = None, predicate: Option[Predicate] = None, `object`: Option[Node] = None) = Graph {
    triples.filter { case Triple(s, p, o) =>
      Seq(subject.map(s.equals), predicate.map(p.equals), `object`.map(o.equals)).flatten.forall(_ == true)
    }
  }

  override lazy val length: Int = triples.length
  override def apply(index: Int): Triple = triples(index)
  override def iterator: Iterator[Triple] = triples.iterator
}

object Graph {

  def apply(triples: Seq[Triple]): Graph = new Graph(triples)
  def apply(first: Triple, rest: Triple*): Graph = new Graph(first +: rest)
  val Empty = Graph(Stream.Empty)

  val marshaller: TurtleMarshaller = new JenaTurtleMarshaller
  def parse(turtle: String): Graph = marshaller.fromTurtle(turtle)
  def toTurtle(graph: Graph): String = marshaller.toTurtle(graph)
}
