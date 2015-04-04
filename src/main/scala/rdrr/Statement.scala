package rdrr

import com.hp.hpl.jena.rdf.model.{Statement => JenaStatement}

case class Statement(statement: JenaStatement) {
  lazy val subject = Resource(statement.getSubject)
  lazy val predicate = Predicate(statement.getPredicate)
  lazy val `object` = RdfNode(statement.getObject)
}
