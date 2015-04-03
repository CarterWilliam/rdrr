package jesc.immutable

import com.hp.hpl.jena.rdf.model.{Property => JenaProperty, RDFNode => JenaNode, Resource => JenaResource}
import jesc.util.JavaHelpers


case class Graph(triples: Stream[Triple]) {
  
  lazy val subjects: Stream[Resource] = triples.map(_.subject)

  def +(t: Triple): Graph = Graph(t #:: triples)

}

object Graph extends JavaHelpers {

  val Empty = Graph(Stream.Empty)

  val serialiser = new JenaSerialiser
  def parse(turtle: String): Graph = serialiser.fromTurtle(turtle)
}
