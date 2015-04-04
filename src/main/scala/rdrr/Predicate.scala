package rdrr

import com.hp.hpl.jena.rdf.model.{ModelFactory, Property => JenaProperty}


case class Predicate(jena: JenaProperty) extends RdfNode {
  lazy val uri = jena.getURI

  override lazy val value = uri
}

object Predicate {
  def apply(uri: String): Predicate =
    Predicate(ModelFactory.createDefaultModel().createProperty(uri))
}
