package jesc

import com.hp.hpl.jena.rdf.model.{ModelFactory, Literal => JenaLiteral}

case class Literal(literal: JenaLiteral) extends RdfNode {
  override lazy val value = literal.getLanguage
}

object Literal {
  def apply(value: String): Literal =
    Literal(ModelFactory.createDefaultModel().createLiteral(value))
  def apply(value: String, language: String): Literal =
    Literal(ModelFactory.createDefaultModel().createLiteral(value, language))
}
