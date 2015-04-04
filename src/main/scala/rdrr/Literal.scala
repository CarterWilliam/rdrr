package rdrr

import com.hp.hpl.jena.rdf.model.{ModelFactory, Literal => JenaLiteral}

case class Literal(jena: JenaLiteral) extends RdfNode {
  override lazy val value = jena.getLanguage
}

object Literal {
  def apply(value: String): Literal =
    Literal(ModelFactory.createDefaultModel().createLiteral(value))
  def apply(value: String, language: String): Literal =
    Literal(ModelFactory.createDefaultModel().createLiteral(value, language))
}
