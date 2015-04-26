package rdrr.immutable.marshallers

import rdrr.immutable._
import org.scalatest.PrivateMethodTester
import org.specs2.mutable.Specification
import org.specs2.specification.Scope
import utilities.TestHelpers

class RdrrTurtleMarshallerSpec extends Specification with PrivateMethodTester {

  "The Turtle Marshaller" can {
    "marshal from" in {
      "triples with Resources from Turtle" in new RdrrTurtleMarshallerScope {
        val turtle = getResource("bieber-is-an-artist.ttl")
        val graph = marshaller.fromTurtle(turtle)
        graph must have size 1
      }

      "triples with resources and literals from Turtle" in new RdrrTurtleMarshallerScope {
        val turtle = getResource("bieber.ttl")
        val graph = marshaller.fromTurtle(turtle)
        graph must have size 3
      }
    }

//    "marshall to Turtle" in new RdrrTurtleMarshallerScope {
//      val graph = Graph(Triple(
//        Resource("https://en.wikipedia.org/wiki/Justin_Bieber"),
//        Predicate("http://www.w3.org/1999/02/22-rdf-syntax-ns#type"),
//        Resource("http://purl.org/ontology/mo/MusicArtist") ) )
//
//      val expected = "<https://en.wikipedia.org/wiki/Justin_Bieber> a <http://purl.org/ontology/mo/MusicArtist> ."
//      marshaller.toTurtle(graph) must beEqualTo (expected).ignoreSpace
//    }

    "extract entities from lines" in {
      "from lines containing resources with full IRIs" in new EntitiesFromLinesScope {
        val resourceString =
          """
            |<https://en.wikipedia.org/wiki/Justin_Bieber>
            |  <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://purl.org/ontology/mo/MusicArtist> .""".stripMargin
        val splitResources = marshaller invokePrivate entitiesFromLines(resourceString.lines.toStream)
        splitResources must be equalTo Stream(
          "<https://en.wikipedia.org/wiki/Justin_Bieber>",
          "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>",
          "<http://purl.org/ontology/mo/MusicArtist>",
          "." )
      }
      "from lines containing prefixed resources" in new EntitiesFromLinesScope {
        val resourceString =
          """
            |@prefix wiki: <https://en.wikipedia.org/wiki/> .
            |@prefix mo: <http://purl.org/ontology/mo/> .
            |wiki:Justin_Bieber a mo:MusicArtist .""".stripMargin
        val splitResources = marshaller invokePrivate entitiesFromLines(resourceString.lines.toStream)
        splitResources must be equalTo Stream(
          "@prefix wiki: <https://en.wikipedia.org/wiki/> .",
          "@prefix mo: <http://purl.org/ontology/mo/> .",
          "wiki:Justin_Bieber", "a", "mo:MusicArtist", ".")
      }
      "from lines containing base-prefixed resources" in new EntitiesFromLinesScope {
        val resourceString =
          """
            |@base wiki: <https://en.wikipedia.org/wiki/> .
            |:Justin_Bieber a mo:MusicArtist .""".stripMargin
        val splitResources = marshaller invokePrivate entitiesFromLines(resourceString.lines.toStream)
        splitResources must be equalTo Stream(
          "@base wiki: <https://en.wikipedia.org/wiki/> .",
          ":Justin_Bieber", "a", "mo:MusicArtist", ".")
      }
      "from lines containing String literals with whitespace" in new EntitiesFromLinesScope {
        val line = "  \"Justin Bieber\" ."
        val splitResources = marshaller invokePrivate entitiesFromLines(Stream(line))
        splitResources must be equalTo Stream("\"Justin Bieber\"", ".")
      }
      "from lines containing language String literals with whitespace" in new EntitiesFromLinesScope {
        val line = "  \"Justin Bieber\"@en-gb ,"
        val splitResources = marshaller invokePrivate entitiesFromLines(Stream(line))
        splitResources must be equalTo Stream("\"Justin Bieber\"@en-gb", ",")
      }
      "from lines containing 'punctuation' characters immediately after" in {
        "a resource" in new EntitiesFromLinesScope {
          val line = "  <http://purl.org/ontology/mo/MusicArtist>."
          val splitResources = marshaller invokePrivate entitiesFromLines(Stream(line))
          splitResources must be equalTo Stream("<http://purl.org/ontology/mo/MusicArtist>", ".")
        }
        "a string literal" in new EntitiesFromLinesScope {
          val line = """  "string literal", """
          val splitResources = marshaller invokePrivate entitiesFromLines(Stream(line))
          splitResources must be equalTo Stream("\"string literal\"", ",")
        }
        "a complex string literal" in new EntitiesFromLinesScope {
          val line = """  "string literal"@en; """
          val splitResources = marshaller invokePrivate entitiesFromLines(Stream(line))
          splitResources must be equalTo Stream("\"string literal\"@en", ";")
        }
      }
      "and throw an exception if line is unrecognised" in new EntitiesFromLinesScope {
        val lines = Stream("  \"unclosed string literal .")
        marshaller invokePrivate entitiesFromLines(lines) must throwA[TurtleParseException].like { case TurtleParseException(message) =>
          message must be equalTo s"RDRR Turtle Marshaller could not parse the line: '${lines.head}'"
        }
      }

    }


    "extract uris" in {
      "from a turtle resource in IRI format" in new IriExtractScope {
        val typeIri = marshaller invokePrivate iriFromTurtleRepresentation("<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>", Map.empty)
        typeIri must be equalTo "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"
      }
      "from a turtle resource in prefix:name format" in new IriExtractScope {
        val prefixes = Map("mo" -> "http://purl.org/ontology/mo/")
        val musicArtistIri = marshaller invokePrivate iriFromTurtleRepresentation("mo:MusicArtist", prefixes)
        musicArtistIri must be equalTo "http://purl.org/ontology/mo/MusicArtist"
      }
      "from the RDF standard abbrieviation'a'" in new IriExtractScope {
        val typeIri = marshaller invokePrivate iriFromTurtleRepresentation("a", Map.empty)
        typeIri must be equalTo "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"
      }
      "throwing an exception if a prefixed resource has no matching prefix" in new IriExtractScope {
        marshaller invokePrivate iriFromTurtleRepresentation("mo:MusicArtist", Map.empty) must throwA[TurtleParseException]
      }
      "throwing an exception if a resource is not in a recognised format" in new IriExtractScope {
        marshaller invokePrivate iriFromTurtleRepresentation("moMusicArtist", Map.empty) must throwA[TurtleParseException]
      }
    }

    "create literals" in {
      "from a turtle string without a language" in new CreateLiteralsScope {
        val literal = marshaller invokePrivate nodeFromTurtle("\"Justin Bieber\"", Map.empty)
        literal must be equalTo StandardStringLiteral("Justin Bieber")
      }
      "from a turtle string with a language" in new CreateLiteralsScope {
        val literal = marshaller invokePrivate nodeFromTurtle("\"Justine Biebere\"@fr", Map.empty)
        literal must be equalTo LanguageStringLiteral("Justine Biebere", "fr")
      }
      "from a turtle string with a custom IRI" in new CreateLiteralsScope {
        val literal = marshaller invokePrivate nodeFromTurtle("\"Justin Bieber\"^^<http://www.crazyencodings.co.uk/encoding>", Map.empty)
        literal must be equalTo NonStandardStringLiteral("Justin Bieber", "http://www.crazyencodings.co.uk/encoding")
      }
      "from a turtle boolean" in new CreateLiteralsScope {
        val literal = marshaller invokePrivate nodeFromTurtle("false", Map.empty)
        literal must be equalTo BooleanLiteral(false)
      }
      "from a turtle integer" in new CreateLiteralsScope {
        val literal = marshaller invokePrivate nodeFromTurtle("684", Map.empty)
        literal must be equalTo IntegerLiteral(684)
      }
      "from an explicitly positive turtle integer" in new CreateLiteralsScope {
        val literal = marshaller invokePrivate nodeFromTurtle("+684", Map.empty)
        literal must be equalTo IntegerLiteral(684)
      }
      "from a negative turtle integer" in new CreateLiteralsScope {
        val literal = marshaller invokePrivate nodeFromTurtle("-684", Map.empty)
        literal must be equalTo IntegerLiteral(-684)
      }
      "from a turtle decimal" in new CreateLiteralsScope {
        val literal = marshaller invokePrivate nodeFromTurtle("6.84", Map.empty)
        literal must be equalTo DecimalLiteral(6.84)
      }
      "from a negative turtle decimal" in new CreateLiteralsScope {
        val literal = marshaller invokePrivate nodeFromTurtle("-6.84", Map.empty)
        literal must be equalTo DecimalLiteral(-6.84)
      }
    }

  }
}

trait RdrrTurtleMarshallerScope extends Scope with TestHelpers {
  val marshaller = new RdrrTurtleMarshaller()
}

trait IriExtractScope extends RdrrTurtleMarshallerScope with PrivateMethodTester {
  val iriFromTurtleRepresentation = PrivateMethod[String]('iriFromTurtleRepresentation)
}

trait CreateLiteralsScope extends RdrrTurtleMarshallerScope with PrivateMethodTester {
  val nodeFromTurtle = PrivateMethod[Node]('nodeFromTurtle)
}

trait EntitiesFromLinesScope extends RdrrTurtleMarshallerScope with PrivateMethodTester {
  val entitiesFromLines = PrivateMethod[Stream[String]]('entitiesFromLines)
}
