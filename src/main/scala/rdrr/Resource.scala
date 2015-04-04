package rdrr

import com.hp.hpl.jena.rdf.model.{ModelFactory, Resource => JenaResource}
import scala.collection.JavaConverters._


case class Resource(jena: JenaResource) extends RdfNode {
  lazy val uri = jena.getURI
  lazy val statements: Stream[Statement] = jena.listProperties.asScala.toStream.map(Statement)

  override lazy val value = uri
}

object Resource {
  def apply(uri: String): Resource =
    Resource(ModelFactory.createDefaultModel().createResource(uri))
}
