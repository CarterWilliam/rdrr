package rdrr.immutable

import rdrr.immutable.marshallers.Prefix

case class Triple(subject: Resource, predicate: Predicate, `object`: Node)

case class RdfGraph(triples: Seq[Triple] = Nil, prefixes: Seq[Prefix] = Nil) extends Seq[Triple] {

  lazy val subjects: Seq[Resource] = triples.map(_.subject).distinct

  def + (∆ : Triple): RdfGraph = RdfGraph(triples :+ ∆, prefixes)
  def + (prefix : Prefix): RdfGraph = RdfGraph(triples, prefixes :+ prefix)

  def contains(subject: Resource, predicate: Predicate, `object`: Node): Boolean =
    triples.contains(Triple(subject, predicate, `object`))

  def filter(subject: Option[Resource] = None, predicate: Option[Predicate] = None, `object`: Option[Node] = None) = RdfGraph {
    triples.filter { case Triple(s, p, o) =>
      Seq(subject.map(s.equals), predicate.map(p.equals), `object`.map(o.equals)).flatten.forall(_ == true)
    }
  }

  override lazy val length: Int = triples.length
  override def apply(index: Int): Triple = triples(index)
  override def iterator: Iterator[Triple] = triples.iterator
  override def filter(predicate: Triple => Boolean) = new RdfGraph(triples.filter(predicate), prefixes)
}

object RdfGraph {

  def apply(first: Triple, rest: Triple*): RdfGraph = new RdfGraph(first +: rest)

  val Empty = RdfGraph(Nil, Nil)
  
}
