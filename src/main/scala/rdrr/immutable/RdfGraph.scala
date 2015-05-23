package rdrr.immutable

import rdrr.immutable.marshallers.Prefix

case class Triple(subject: Resource, predicate: Predicate, `object`: Node)

case class RdfGraph(triples: Seq[Triple] = Nil, prefixes: Seq[Prefix] = Nil) extends Seq[Triple] {

  lazy val subjects: Seq[Resource] = triples.map(_.subject).distinct

  def + (∆ : Triple): RdfGraph = RdfGraph(triples :+ ∆, prefixes)
  def + (prefix : Prefix): RdfGraph = RdfGraph(triples, prefixes :+ prefix)

  def contains(subject: Resource, predicate: Predicate, `object`: Node): Boolean =
    triples.contains(Triple(subject, predicate, `object`))

  override lazy val length: Int = triples.length
  override def apply(index: Int): Triple = triples(index)
  override def iterator: Iterator[Triple] = triples.iterator
  
  override def filter(predicate: Triple => Boolean) = RdfGraph(triples.filter(predicate), prefixes)

  def transform(partialFunction: PartialFunction[Triple, Triple]) =
    RdfGraph(triples.collect(partialFunction).distinct, prefixes)

  def ++ (graph: RdfGraph) = RdfGraph((triples ++ graph.triples).distinct, (prefixes ++ graph.prefixes).distinct)
  def :+ (∆ : Triple) = RdfGraph((triples :+ ∆).distinct, prefixes)
  def +: (∆ : Triple) = RdfGraph((∆ +: triples).distinct, prefixes)
}

object RdfGraph {

  def apply(first: Triple, rest: Triple*): RdfGraph = new RdfGraph(first +: rest)

  val Empty = RdfGraph(Nil, Nil)
  
}
