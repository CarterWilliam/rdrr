package jesc

import com.hp.hpl.jena.rdf.model.{RDFNode => JenaNode}

abstract class RdfNode {
  def value: String
}

object RdfNode {
  def apply(jenaNode: JenaNode) = {
    if (jenaNode.isLiteral)
      Literal(jenaNode.asLiteral())
    else
      Resource(jenaNode.asResource())
  }
}
