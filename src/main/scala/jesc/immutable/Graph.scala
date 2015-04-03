package jesc.immutable

import com.hp.hpl.jena.rdf.model.{Property => JenaProperty, RDFNode => JenaNode, Resource => JenaResource}
import jesc.immutable.marshallers.JenaMarshaller
import jesc.util.JavaHelpers


case class Graph(triples: Stream[Triple]) {
  
  lazy val subjects: Stream[Resource] = triples.map(_.subject).distinct

  def + (t: Triple): Graph = Graph(t #:: triples)

}

object Graph extends JavaHelpers {

  val Empty = Graph(Stream.Empty)

  val marshaller = new JenaMarshaller
  def parse(turtle: String): Graph = marshaller.fromTurtle(turtle)
  def toTurtle(graph: Graph): String = marshaller.toTurtle(graph)
}
