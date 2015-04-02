package jesc

import com.hp.hpl.jena.rdf.model.{ModelFactory, Resource => JenaResource}
import scala.collection.JavaConverters._


case class Resource(resource: JenaResource) extends RdfNode {
  lazy val uri = resource.getURI
  lazy val statements: Stream[Statement] = resource.listProperties.asScala.toStream.map(Statement)

  override lazy val value = uri
}

object Resource {
  def apply(uri: String): Resource =
    Resource(ModelFactory.createDefaultModel().createResource(uri))
}
