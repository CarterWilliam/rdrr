package rdrr.immutable.marshallers

import rdrr.immutable._
import org.scalatest.PrivateMethodTester
import org.specs2.mutable.Specification
import org.specs2.specification.Scope
import rdrr.immutable.marshallers.RdrrTurtleUnmarshaller.{EmptyTriple, ParserState}
import utilities.TestHelpers

class RdrrTurtleMarshallerSpec extends Specification with PrivateMethodTester {

  "The Turtle Marshaller" can {

    "marshal from" in {
      "triples with Resources from Turtle" in new RdrrTurtleUnmarshallerScope {
        val turtle = getResource("bieber-is-an-artist.ttl")
        val graph = marshaller.fromTurtle(turtle)
        graph must have size 1
      }

      "triples with resources and literals from Turtle" in new RdrrTurtleUnmarshallerScope {
        val turtle = getResource("bieber.ttl")
        val graph = marshaller.fromTurtle(turtle)
        graph must have size 3
      }
    }

    "extract entities from lines" in {

      "containing resources with full IRIs" in new EntitiesFromLinesScope {
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

      "containing prefixed resources" in new EntitiesFromLinesScope {
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

      "containing base-prefixed resources" in new EntitiesFromLinesScope {
        val resourceString =
          """
            |@base wiki: <https://en.wikipedia.org/wiki/> .
            |:Justin_Bieber a mo:MusicArtist .""".stripMargin
        val splitResources = marshaller invokePrivate entitiesFromLines(resourceString.lines.toStream)
        splitResources must be equalTo Stream(
          "@base wiki: <https://en.wikipedia.org/wiki/> .",
          ":Justin_Bieber", "a", "mo:MusicArtist", ".")
      }

      "containing String literals" in {

        "with whitespace" in new EntitiesFromLinesScope {
          val line = "  \"Justin Bieber\" ."
          val splitResources = marshaller invokePrivate entitiesFromLines(Stream(line))
          splitResources must be equalTo Stream("\"Justin Bieber\"", ".")
        }

        "with whitespace and language tag" in new EntitiesFromLinesScope {
          val line = "  \"Justin Bieber\"@en-gb ,"
          val splitResources = marshaller invokePrivate entitiesFromLines(Stream(line))
          splitResources must be equalTo Stream("\"Justin Bieber\"@en-gb", ",")
        }

        "enclosed with apostrophes" in new EntitiesFromLinesScope {
          val line = "  'Justin Bieber'@en ."
          val splitResources = marshaller invokePrivate entitiesFromLines(Stream(line))
          splitResources must be equalTo Stream("'Justin Bieber'@en", ".")
        }

        "enclosed with triple-quotes" in new EntitiesFromLinesScope {
          val line = " \"\"\" '''''\" \"''''' \"\"\" ."
          val splitResources = marshaller invokePrivate entitiesFromLines(Stream(line))
          splitResources must be equalTo Stream("\"\"\" '''''\" \"''''' \"\"\"", ".")
        }

        "enclosed with triple-apostrophes" in new EntitiesFromLinesScope {
          val line = """ ''' quotes '@en ' quotes ''',"""
          val splitResources = marshaller invokePrivate entitiesFromLines(Stream(line))
          splitResources must be equalTo Stream("''' quotes '@en ' quotes '''", ",")
        }

        "enclosed with triple apostrophes and spanning several lines" in new EntitiesFromLinesScope {
          val multilineString =
            """ ''' start
              |end'''@en . """.stripMargin
          val splitResources = marshaller invokePrivate entitiesFromLines(multilineString.lines.toStream)
          splitResources must be equalTo Stream("''' start\nend'''@en", ".")
        }

        "enclosed with triple quotes and spanning several lines" in new EntitiesFromLinesScope {
          val tripleQuote = "\"\"\""
          val multilineString =
            s""" $tripleQuote start
              |end$tripleQuote@en. """.stripMargin
          val splitResources = marshaller invokePrivate entitiesFromLines(multilineString.lines.toStream)
          splitResources must be equalTo Stream(s"$tripleQuote start\nend$tripleQuote@en", ".")
        }
      }

      "containing 'punctuation' characters immediately after" in {

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

      "ignoring full line comments" in new EntitiesFromLinesScope {
        val lines = Stream("<Justin_Bieber> a ", "# fool haha!", "mo:Artist", ".")
        val splitResources = marshaller invokePrivate entitiesFromLines(lines)
        splitResources must be equalTo Stream("<Justin_Bieber>", "a", "mo:Artist", ".")
      }

      "ignoring inline comments" in new EntitiesFromLinesScope {
        val lines = Stream("<Justin_Bieber> a # fool haha!", "mo:Artist", ".")
        val splitResources = marshaller invokePrivate entitiesFromLines(lines)
        splitResources must be equalTo Stream("<Justin_Bieber>", "a", "mo:Artist", ".")
      }

      "and throw an exception if the line is unrecognised" in new EntitiesFromLinesScope {
        val lines = Stream("  \"unclosed string literal .")
        marshaller invokePrivate entitiesFromLines(lines) must throwA[TurtleParseException].like { case TurtleParseException(message) =>
          message must be equalTo s"RDRR Turtle Marshaller could not parse the line: '${lines.head}'"
        }
      }

    }


    "extract uris" in {
      "from a turtle resource in IRI format" in new IriExtractScope {
        val typeIri = marshaller invokePrivate iriFromTurtle("<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>", ParserState.Empty)
        typeIri must be equalTo "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"
      }
      "from a turtle resource in prefix:name format" in new IriExtractScope {
        val parserState = ParserState.Empty + Prefix("mo", "http://purl.org/ontology/mo/")
        val musicArtistIri = marshaller invokePrivate iriFromTurtle("mo:MusicArtist", parserState)
        musicArtistIri must be equalTo "http://purl.org/ontology/mo/MusicArtist"
      }
      "from a relative URI turtle resource" in new IriExtractScope {
        val parserState = ParserState.Empty + BasePrefix("http://purl.org/ontology/mo/")
        val musicArtistIri = marshaller invokePrivate iriFromTurtle("<MusicArtist>", parserState)
        musicArtistIri must be equalTo "http://purl.org/ontology/mo/MusicArtist"
      }
      "from the RDF standard abbrieviation'a'" in new IriExtractScope {
        val typeIri = marshaller invokePrivate iriFromTurtle("a", ParserState.Empty)
        typeIri must be equalTo "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"
      }
      "throwing an exception if a prefixed resource has no matching prefix" in new IriExtractScope {
        marshaller invokePrivate iriFromTurtle("mo:MusicArtist", ParserState.Empty) must throwA[TurtleParseException]
      }
      "throwing an exception if a resource is not in a recognised format" in new IriExtractScope {
        marshaller invokePrivate iriFromTurtle("moMusicArtist", ParserState.Empty) must throwA[TurtleParseException]
      }
    }

    "create literals" in {
      "from a turtle string without a language" in new CreateLiteralsScope {
        val literal = marshaller invokePrivate nodeFromTurtle("\"Justin Bieber\"", ParserState.Empty)
        literal must be equalTo StandardStringLiteral("Justin Bieber")
      }
      "from a turtle string with a language" in new CreateLiteralsScope {
        val literal = marshaller invokePrivate nodeFromTurtle("\"Justine Biebere\"@fr", ParserState.Empty)
        literal must be equalTo LanguageStringLiteral("Justine Biebere", "fr")
      }
      "from a turtle string with a custom IRI" in new CreateLiteralsScope {
        val literal = marshaller invokePrivate nodeFromTurtle("\"Justin Bieber\"^^<http://www.crazyencodings.co.uk/encoding>", ParserState.Empty)
        literal must be equalTo NonStandardStringLiteral("Justin Bieber", "http://www.crazyencodings.co.uk/encoding")
      }
      "from a turtle boolean" in new CreateLiteralsScope {
        val literal = marshaller invokePrivate nodeFromTurtle("false", ParserState.Empty)
        literal must be equalTo BooleanLiteral(false)
      }
      "from a turtle integer" in new CreateLiteralsScope {
        val literal = marshaller invokePrivate nodeFromTurtle("684", ParserState.Empty)
        literal must be equalTo IntegerLiteral(684)
      }
      "from an explicitly positive turtle integer" in new CreateLiteralsScope {
        val literal = marshaller invokePrivate nodeFromTurtle("+684", ParserState.Empty)
        literal must be equalTo IntegerLiteral(684)
      }
      "from a negative turtle integer" in new CreateLiteralsScope {
        val literal = marshaller invokePrivate nodeFromTurtle("-684", ParserState.Empty)
        literal must be equalTo IntegerLiteral(-684)
      }
      "from a turtle decimal" in new CreateLiteralsScope {
        val literal = marshaller invokePrivate nodeFromTurtle("6.84", ParserState.Empty)
        literal must be equalTo DecimalLiteral(6.84)
      }
      "from a negative turtle decimal" in new CreateLiteralsScope {
        val literal = marshaller invokePrivate nodeFromTurtle("-6.84", ParserState.Empty)
        literal must be equalTo DecimalLiteral(-6.84)
      }
    }

  }


  "The ParserState" should {

    "return the correct state when a standard prefix is added" in  {
      val musicOntologyPrefix = Prefix("mo", "http://purl.org/ontology/mo/")
      val expectedState = ParserState(Seq(musicOntologyPrefix), None, EmptyTriple)
      ParserState.Empty + musicOntologyPrefix must be equalTo expectedState
    }

    "overwrite older prefix mappings if they have the same prefix" in {
      val musicOntologyPrefix = Prefix("mo", "http://purl.org/ontology/mo/")
      val wikipediaPrefix = Prefix("wiki", "http://en.wikipedia.org/wiki/")
      val kanzakiMusicOntologyPrefix = Prefix("mo", "http://www.kanzaki.com/ns/music#")

      val initialState = ParserState(Seq(musicOntologyPrefix, wikipediaPrefix), None, EmptyTriple)
      val expectedState = ParserState(Seq(wikipediaPrefix, kanzakiMusicOntologyPrefix), None, EmptyTriple)

      initialState + kanzakiMusicOntologyPrefix must be equalTo expectedState
    }

    "return the correct state when a base prefix is added" in {
      val musicOntologyBasePrefix = BasePrefix("http://purl.org/ontology/mo/")
      val expectedState = ParserState(Nil, Some(musicOntologyBasePrefix), EmptyTriple)
      ParserState.Empty + musicOntologyBasePrefix must be equalTo expectedState
    }

    "overwrite older base prefixes" in {
      val basePrefix1 = BasePrefix("http://base.prefix/1/")
      val basePrefix2 = BasePrefix("http://base.prefix/2/")
      val initialState = ParserState(Nil, Some(basePrefix1), EmptyTriple)
      val expectedState = ParserState(Nil, Some(basePrefix2), EmptyTriple)
      initialState + basePrefix2 must be equalTo expectedState
    }
  }
}

trait RdrrTurtleUnmarshallerScope extends Scope with TestHelpers {
  val marshaller = RdrrTurtleUnmarshaller
}

trait IriExtractScope extends RdrrTurtleUnmarshallerScope with PrivateMethodTester {
  val iriFromTurtle = PrivateMethod[String]('iriFromTurtle)
}

trait CreateLiteralsScope extends RdrrTurtleUnmarshallerScope with PrivateMethodTester {
  val nodeFromTurtle = PrivateMethod[Node]('nodeFromTurtle)
}

trait EntitiesFromLinesScope extends RdrrTurtleUnmarshallerScope with PrivateMethodTester {
  val entitiesFromLines = PrivateMethod[Stream[String]]('entitiesFromLines)
}

trait ParserStateScope extends Scope {

}
