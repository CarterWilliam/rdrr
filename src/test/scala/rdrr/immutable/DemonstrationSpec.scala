package rdrr.immutable

import org.scalatest.{FunSpec, MustMatchers}
import rdrr.immutable.marshallers.{RdrrTurtleMarshaller, RdrrTurtleUnmarshaller}

class DemonstrationSpec extends FunSpec with MustMatchers {

  val Brentford = Resource("http://www.bbc.co.uk/things/cf72a7ad-3054-8d4c-a98c-fb0ec5888922#id")

  describe ("The Graph navigation DSL") {

    it ("can fetch and transform child nodes of a graph node") {
      val competitionIris = graph.collect {
        case Triple(Brentford, SportOntology.competesIn, competition) => competition
      }
      competitionIris must have size 4

      val homeGroundIri = graph.collectFirst {
        case Triple(Brentford, SportOntology.hasHome, homeGround) => homeGround
      }
      homeGroundIri must contain (Resource("http://www.bbc.co.uk/things/9ae46e8b-fea7-4e4a-858e-240c3bc4845f#id"))
    }

    it ("can chain multiple queries") {
      def brentfordHasLocator(locator: Resource) = graph.contains(Brentford, CMSOntology.locator, locator)
      val locatorTypes = graph.collect {
        case Triple(locator, RdfOntology.`type`, locatorType) if brentfordHasLocator(locator) => locatorType
      }
      locatorTypes must contain only (CMSOntology.CPSLocator, CMSOntology.SportsStatsLocator)
    }

  }

  val turtle =
    """
      |@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
      |@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
      |@prefix owl: <http://www.w3.org/2002/07/owl#> .
      |@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
      |@prefix result: <http://purl.org/ontology/rdf-result/> .
      |@prefix dcterms: <http://purl.org/dc/terms/> .
      |@prefix dc: <http://purl.org/dc/elements/1.1/> .
      |@prefix sport: <http://www.bbc.co.uk/ontologies/sport/> .
      |@prefix news: <http://www.bbc.co.uk/ontologies/news/> .
      |@prefix nsl: <http://purl.org/ontology/storyline/> .
      |@prefix cwork: <http://www.bbc.co.uk/ontologies/creativework/> .
      |@prefix core: <http://www.bbc.co.uk/ontologies/coreconcepts/> .
      |@prefix bbc: <http://www.bbc.co.uk/ontologies/bbc/> .
      |@prefix tagging: <http://www.bbc.co.uk/ontologies/tagging/> .
      |@prefix cms: <http://www.bbc.co.uk/ontologies/cms/> .
      |@prefix provenance: <http://www.bbc.co.uk/ontologies/provenance/> .
      |@prefix pol: <http://www.bbc.co.uk/ontologies/politics/> .
      |@prefix biz: <http://www.bbc.co.uk/ontologies/business/> .
      |@prefix skos: <http://www.w3.org/2004/02/skos/core#> .
      |@prefix curric: <http://www.bbc.co.uk/ontologies/curriculum/> .
      |@prefix geo: <http://www.w3.org/2003/01/geo/wgs84_pos#> .
      |@prefix geo-pol: <http://www.bbc.co.uk/ontologies/geopolitical/> .
      |@prefix geoname: <http://www.geonames.org/ontology#> .
      |@prefix omgeo: <http://www.ontotext.com/owlim/geo#> .
      |@prefix luc: <http://www.ontotext.com/owlim/lucene#> .
      |@prefix asset: <http://www.bbc.co.uk/ontologies/asset/> .
      |@prefix domain: <http://www.bbc.co.uk/ontologies/domain/> .
      |@prefix tag: <http://www.bbc.co.uk/ontologies/tag/> .
      |@prefix onto: <http://www.ontotext.com/> .
      |@prefix sesame: <http://www.openrdf.org/schema/sesame#> .
      |@prefix fn: <http://www.w3.org/2005/xpath-functions#> .
      |
      |
      |result:this result:item <http://www.bbc.co.uk/things/cf72a7ad-3054-8d4c-a98c-fb0ec5888922#id> .
      |
      |<http://www.bbc.co.uk/things/cf72a7ad-3054-8d4c-a98c-fb0ec5888922#id> a sport:CompetitiveSportingOrganisation , tagging:TagConcept , cms:ManagedThing ;
      |	domain:canonicalName "Brentford"^^xsd:string ;
      |	domain:name "Brentford"^^xsd:string ;
      |	domain:shortName "Brentford"^^xsd:string ;
      |	sport:competesIn <http://www.bbc.co.uk/things/0146f8ae-30d2-4fb1-9207-4b7db702f257#id> , <http://www.bbc.co.uk/things/ad6c7f89-d439-41c2-9c4e-b8f3afd542ec#id> , <http://www.bbc.co.uk/things/9e935eb6-835d-4fb0-9591-9df59ea55fcd#id> , <http://www.bbc.co.uk/things/573d4822-6331-4f53-8871-812af2958a9a#id> ;
      |	sport:discipline <http://www.bbc.co.uk/things/ba6e1118-f874-054e-b159-b797c16e9250#id> ;
      |	domain:document <http://www.bbc.co.uk/sport/football/teams/brentford> , <http://open.live.bbc.co.uk/esp-service/moreover/newsdesk4/bdd9aa9a3618158f?output=rss;recipe=sport_refresh> , <http://www.brentfordfc.co.uk/> ;
      |	domain:externalId <http://dbpedia.org/resource/Brentford_F.C.> , <urn:sports-stats:137318251> ;
      |	core:preferredLabel "Brentford"@en-gb ;
      |	cms:locator <urn:sports-stats:137318251> , <urn:sports-data:TFBB94> ;
      |	domain:slug "brentford"^^xsd:string ;
      |	core:sameAs <http://dbpedia.org/resource/Brentford_F.C.> , <http://rdf.freebase.com/ns/m.02b0y3> , <http://www.wikidata.org/entity/Q19571> ;
      |	core:slug "brentford"^^xsd:string ;
      |	core:disambiguationHint "English Football Team"^^xsd:string ;
      |	core:primaryTopicOf <http://www.bbc.co.uk/sport/football/teams/brentford> , <http://www.brentfordfc.co.uk/> ;
      |	core:shortLabel "Brentford"^^xsd:string ;
      |	sport:hasHome <http://www.bbc.co.uk/things/9ae46e8b-fea7-4e4a-858e-240c3bc4845f#id> ;
      |	<http://www.bbc.co.uk/ontologies/applicationlogic-sport/videCode> "BREN"^^xsd:string ;
      |	<http://www.bbc.co.uk/ontologies/applicationlogic-sport/name> "Brentford"^^xsd:string ;
      |	bbc:coveredBy <http://www.bbc.co.uk/things/32308873-9c53-4071-97b5-4aa7129a4bc5#id> .
      |
      |<urn:sports-stats:137318251> a cms:Sports-StatsLocator .
      |<urn:sports-data:TFBB94> a cms:CPSLocator .
      |
    """.stripMargin

  val graph = (new RdrrTurtleUnmarshaller).fromTurtle(turtle)


  object RdfOntology {
    val `type` = Predicate("http://www.w3.org/1999/02/22-rdf-syntax-ns#type")
  }

  object SportOntology {
    val prefix = "http://www.bbc.co.uk/ontologies/sport/"
    val competesIn = Predicate("http://www.bbc.co.uk/ontologies/sport/competesIn")
    val hasHome = Predicate("http://www.bbc.co.uk/ontologies/sport/hasHome")
  }

  object CMSOntology {
    val prefix = "http://www.bbc.co.uk/ontologies/cms/"
    val locator = Predicate("http://www.bbc.co.uk/ontologies/cms/locator")
    val SportsStatsLocator = Resource("http://www.bbc.co.uk/ontologies/cms/Sports-StatsLocator")
    val CPSLocator = Resource("http://www.bbc.co.uk/ontologies/cms/CPSLocator")
  }

}
