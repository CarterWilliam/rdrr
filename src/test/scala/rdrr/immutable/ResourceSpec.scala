package rdrr.immutable

import org.specs2.mutable.Specification

class ResourceSpec extends Specification {

  "A Node" should {
    "be castable to a resource object" in {
      val node: GraphNode = Resource("iri")
      node.as[Resource] must be equalTo Resource("iri")
    }
    "be castable to a literal object" in {
      val node: GraphNode = StandardStringLiteral("string")
      node.as[Literal].value must be equalTo "string"
    }
    "throw an excpetion if cast to the wrong type" in {
      val node: GraphNode = StandardStringLiteral("string")
      node.as[Resource] must throwA[WrongNodeTypeException]
    }
  }
}
