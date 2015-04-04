package rdrr

import org.specs2.mutable.Specification

class PredicateSpec extends Specification {

  "Predicates" can {
    "be created from a URI string" in {
      Predicate("http://www.bbc.co.uk/ontologies/bbc/webDocumentCategory").value must be equalTo
        "http://www.bbc.co.uk/ontologies/bbc/webDocumentCategory"
    }
  }

}
