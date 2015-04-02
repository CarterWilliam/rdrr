package jesc

import com.hp.hpl.jena.rdf.model.{Literal => JenaLiteral}

case class Literal(literal: JenaLiteral) extends RdfNode {
  override lazy val value = literal.getValue.toString
}
