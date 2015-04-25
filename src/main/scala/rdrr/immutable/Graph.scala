package rdrr.immutable

import rdrr.immutable.marshallers.{TurtleMarshaller, JenaTurtleMarshaller}


case class Graph(triples: Seq[Triple]) {

  lazy val subjects: Seq[Resource] = triples.map(_.subject).distinct

  def + (∆ : Triple): Graph = Graph(triples :+ ∆)

  def contains(∆ : Triple): Boolean = triples.contains(∆)
  def contains(subject: Resource, predicate: Predicate, `object`: Node): Boolean =
    triples.contains(Triple(subject, predicate, `object`))

  def filter(constraint: Triple => Boolean) = Graph(triples filter constraint)
  def filter(subject: Option[Resource] = None, predicate: Option[Predicate] = None, `object`: Option[Node] = None) = Graph {
    triples.filter { case Triple(s, p, o) =>
      Seq(subject.map(s.equals), predicate.map(p.equals), `object`.map(o.equals)).flatten.forall(_ == true)
    }
  }

  def collect[T](transform: PartialFunction[Triple, T]): Seq[T] = triples collect transform
}

object Graph {

  def apply(first: Triple, rest: Triple*): Graph = Graph(first +: rest)
  val Empty = Graph(Stream.Empty)

  val marshaller: TurtleMarshaller = new JenaTurtleMarshaller
  def parse(turtle: String): Graph = marshaller.fromTurtle(turtle)
  def toTurtle(graph: Graph): String = marshaller.toTurtle(graph)
}
