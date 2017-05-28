package rdrr.immutable

case class Triple(subject: RdfResource, predicate: Resource, `object`: GraphNode)

class Graph(triples: Seq[Triple]) extends Seq[Triple] {

  lazy val subjects: Seq[RdfResource] = triples.map(_.subject).distinct

  def contains(subject: RdfResource, predicate: Resource, `object`: GraphNode): Boolean =
    triples.contains(Triple(subject, predicate, `object`))

  override lazy val length: Int = triples.length
  override def apply(index: Int): Triple = triples(index)
  override def iterator: Iterator[Triple] = triples.iterator

  def + (∆ : Triple): Graph = Graph((triples :+ ∆).distinct)
  def :+ (∆ : Triple): Graph = Graph((triples :+ ∆).distinct)
  def +: (∆ : Triple): Graph = Graph((∆ +:triples).distinct)
  def ++ (other : Graph): Graph = Graph((triples ++ other).distinct)

  override def filter(predicate: Triple => Boolean): Graph = Graph(triples.filter(predicate))
  def transform(partialFunction: PartialFunction[Triple, Triple]): Graph =
    Graph(triples.collect(partialFunction).distinct)

}

object Graph {

  def apply(triples: Seq[Triple]): Graph = new Graph(triples)
  def apply(first: Triple, rest: Triple*): Graph = new Graph(first +: rest)

  val Empty = Graph(Stream.Empty)

}
