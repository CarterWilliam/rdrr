package rdrr.immutable

import rdrr.immutable.marshallers.Prefix

object RdfOntology  {
  val prefix = Prefix("rdf", "http://www.w3.org/1999/02/22-rdf-syntax-ns#")

  val `type` = Resource("http://www.w3.org/1999/02/22-rdf-syntax-ns#type")
  val first = Resource("http://www.w3.org/1999/02/22-rdf-syntax-ns#first")
  val rest = Resource("http://www.w3.org/1999/02/22-rdf-syntax-ns#rest")

  val nil = Resource("http://www.w3.org/1999/02/22-rdf-syntax-ns#nil")
}
