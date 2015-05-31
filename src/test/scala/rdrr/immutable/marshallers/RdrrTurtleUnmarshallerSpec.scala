package rdrr.immutable.marshallers

import rdrr.immutable._
import org.scalatest.PrivateMethodTester
import org.specs2.mutable.Specification
import org.specs2.specification.Scope
import rdrr.immutable.marshallers.RdrrTurtleUnmarshaller.{Subject, ParserState, EmptyTriple}
import utilities.TestHelpers

class RdrrTurtleUnmarshallerSpec extends Specification with PrivateMethodTester {

  "The RDRR Turtle Unmarshaller" should {

    "extract entities from lines" in {

      "containing resources with full IRIs" in new EntitiesFromLinesScope {
        val resourceString =
          """
            |<https://en.wikipedia.org/wiki/Justin_Bieber>
            |  <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://purl.org/ontology/mo/MusicArtist> .""".stripMargin
        val splitResources = unmarshaller invokePrivate entitiesFromLines(resourceString.lines.toStream)
        splitResources must be equalTo Stream(
          "<https://en.wikipedia.org/wiki/Justin_Bieber>",
          "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>",
          "<http://purl.org/ontology/mo/MusicArtist>",
          "." )
      }

      "containing prefixed resources in standard and SPARQL syntax" in new EntitiesFromLinesScope {
        val resourceString =
          """
            |@prefix wiki: <https://en.wikipedia.org/wiki/> .
            |PREFIX mo: <http://purl.org/ontology/mo/>
            |wiki:Justin_Bieber a mo:MusicArtist .""".stripMargin
        val splitResources = unmarshaller invokePrivate entitiesFromLines(resourceString.lines.toStream)
        splitResources must be equalTo Stream(
          "@prefix wiki: <https://en.wikipedia.org/wiki/> .",
          "PREFIX mo: <http://purl.org/ontology/mo/>",
          "wiki:Justin_Bieber", "a", "mo:MusicArtist", ".")
      }

      "containing base-prefixed resources" in {
        "in standard syntax" in new EntitiesFromLinesScope {
          val resourceString =
          """
            |@base wiki: <https://en.wikipedia.org/wiki/> .
            |:Justin_Bieber a mo:MusicArtist .""".stripMargin
          val splitResources = unmarshaller invokePrivate entitiesFromLines(resourceString.lines.toStream)
          splitResources must be equalTo Stream(
          "@base wiki: <https://en.wikipedia.org/wiki/> .",
          ":Justin_Bieber", "a", "mo:MusicArtist", ".")
        }
        "in SPARQL syntax" in new EntitiesFromLinesScope {
          val resourceString =
            """
              |BASE wiki: <https://en.wikipedia.org/wiki/>
              |:Justin_Bieber a mo:MusicArtist .""".stripMargin
          val splitResources = unmarshaller invokePrivate entitiesFromLines(resourceString.lines.toStream)
          splitResources must be equalTo Stream(
            "BASE wiki: <https://en.wikipedia.org/wiki/>",
            ":Justin_Bieber", "a", "mo:MusicArtist", ".")
        }
      }

      "containing labeled blank nodes" in new EntitiesFromLinesScope {
        val resourceString = " _:someone a _:something."
        val splitResources = unmarshaller invokePrivate entitiesFromLines(Stream(resourceString))
        splitResources must be equalTo Stream("_:someone", "a", "_:something", ".")
      }

      "containing unlabeled blank nodes" in new EntitiesFromLinesScope {
        val resourceString = " [] a []."
        val splitResources = unmarshaller invokePrivate entitiesFromLines(Stream(resourceString))
        splitResources must be equalTo Stream("[]", "a", "[]", ".")
      }

      "containing unlabeled blank nodes with nested triples" in new EntitiesFromLinesScope {
        val resourceString = " [ a foaf:Person ]."
        val splitResources = unmarshaller invokePrivate entitiesFromLines(Stream(resourceString))
        splitResources must be equalTo Stream("[", "a", "foaf:Person", "]", ".")
      }

      "containing String literals" in {

        "with whitespace" in new EntitiesFromLinesScope {
          val line = "  \"Justin Bieber\" ."
          val splitResources = unmarshaller invokePrivate entitiesFromLines(Stream(line))
          splitResources must be equalTo Stream("\"Justin Bieber\"", ".")
        }

        "with whitespace and language tag" in new EntitiesFromLinesScope {
          val line = "  \"Justin Bieber\"@en-gb ,"
          val splitResources = unmarshaller invokePrivate entitiesFromLines(Stream(line))
          splitResources must be equalTo Stream("\"Justin Bieber\"@en-gb", ",")
        }

        "enclosed with apostrophes" in new EntitiesFromLinesScope {
          val line = "  'Justin Bieber'@en ."
          val splitResources = unmarshaller invokePrivate entitiesFromLines(Stream(line))
          splitResources must be equalTo Stream("'Justin Bieber'@en", ".")
        }

        "enclosed with triple-quotes" in new EntitiesFromLinesScope {
          val line = " \"\"\" '''''\" \"''''' \"\"\" ."
          val splitResources = unmarshaller invokePrivate entitiesFromLines(Stream(line))
          splitResources must be equalTo Stream("\"\"\" '''''\" \"''''' \"\"\"", ".")
        }

        "enclosed with triple-apostrophes" in new EntitiesFromLinesScope {
          val line = """ ''' quotes '@en ' quotes ''',"""
          val splitResources = unmarshaller invokePrivate entitiesFromLines(Stream(line))
          splitResources must be equalTo Stream("''' quotes '@en ' quotes '''", ",")
        }

        "enclosed with triple apostrophes and spanning several lines" in new EntitiesFromLinesScope {
          val multilineString =
            """ ''' start
              |end'''@en . """.stripMargin
          val splitResources = unmarshaller invokePrivate entitiesFromLines(multilineString.lines.toStream)
          splitResources must be equalTo Stream("''' start\nend'''@en", ".")
        }

        "enclosed with triple quotes and spanning several lines" in new EntitiesFromLinesScope {
          val tripleQuote = "\"\"\""
          val multilineString =
            s""" $tripleQuote start
              |end$tripleQuote@en. """.stripMargin
          val splitResources = unmarshaller invokePrivate entitiesFromLines(multilineString.lines.toStream)
          splitResources must be equalTo Stream(s"$tripleQuote start\nend$tripleQuote@en", ".")
        }
      }

      "containing 'punctuation' characters immediately after" in {

        "a resource" in new EntitiesFromLinesScope {
          val line = "  <http://purl.org/ontology/mo/MusicArtist>."
          val splitResources = unmarshaller invokePrivate entitiesFromLines(Stream(line))
          splitResources must be equalTo Stream("<http://purl.org/ontology/mo/MusicArtist>", ".")
        }

        "a string literal" in new EntitiesFromLinesScope {
          val line = """  "string literal", """
          val splitResources = unmarshaller invokePrivate entitiesFromLines(Stream(line))
          splitResources must be equalTo Stream("\"string literal\"", ",")
        }

        "a complex string literal" in new EntitiesFromLinesScope {
          val line = """  "string literal"@en; """
          val splitResources = unmarshaller invokePrivate entitiesFromLines(Stream(line))
          splitResources must be equalTo Stream("\"string literal\"@en", ";")
        }

      }

      "ignoring full line comments" in new EntitiesFromLinesScope {
        val lines = Stream("<Justin_Bieber> a ", "# fool haha!", "mo:Artist", ".")
        val splitResources = unmarshaller invokePrivate entitiesFromLines(lines)
        splitResources must be equalTo Stream("<Justin_Bieber>", "a", "mo:Artist", ".")
      }

      "ignoring inline comments" in new EntitiesFromLinesScope {
        val lines = Stream("<Justin_Bieber> a # fool haha!", "mo:Artist", ".")
        val splitResources = unmarshaller invokePrivate entitiesFromLines(lines)
        splitResources must be equalTo Stream("<Justin_Bieber>", "a", "mo:Artist", ".")
      }

      "and throw an exception if the line is unrecognised" in new EntitiesFromLinesScope {
        val lines = Stream("  \"unclosed string literal .")
        unmarshaller invokePrivate entitiesFromLines(lines) must throwA[TurtleParseException].like { case TurtleParseException(message) =>
          message must be equalTo s"RDRR Turtle Marshaller could not parse the line: '${lines.head}'"
        }
      }

    }

    "partition the entity stream into scopes" in {

      "when the blank node scope is flat" in new PartitionBlankNodeScopeScope {
        val entities = Stream("a", "foaf:Person", "]", ".")
        val scopes = unmarshaller invokePrivate partitionBlankNodeScope(entities, 0)
        scopes must be equalTo (Stream("a", "foaf:Person"), Stream("."))
      }

      "when the blank node scope is nested" in new PartitionBlankNodeScopeScope {
        val entities = Stream("a", "[", "foaf:name", "[]", "]", ";", "foaf:knows", "[]", "]", ".")
        val scopes = unmarshaller invokePrivate partitionBlankNodeScope(entities, 0)
        scopes must be equalTo (Stream("a", "[", "foaf:name", "[]", "]", ";", "foaf:knows", "[]"), Stream("."))
      }

    }

    "convert collections into blank node scopes" in new ConvertCollectionScope {
      val entities = Stream("1", "<resource>")
      val expected = Stream("[", "rdf:first", "1", ";", "rdf:rest", "[", "rdf:first", "<resource>", ";", "rdf:rest",
        "rdf:nil", "]", "]")
      val actual = unmarshaller invokePrivate convertCollectionToBlankNodes(entities)
      actual must be equalTo expected
    }

    "convert nested collections into blank node scopes" in new ConvertCollectionScope {
      val entities = Stream("1", "(",  "2", ")", "3")
      val expected = Stream("[", "rdf:first", "1", ";", "rdf:rest", "[", "rdf:first", "(",  "2", ")", ";", "rdf:rest",
        "[", "rdf:first", "3", ";", "rdf:rest", "rdf:nil", "]", "]", "]")
      val actual = unmarshaller invokePrivate convertCollectionToBlankNodes(entities)
      expected.size; actual.size
      actual must be equalTo expected
    }

    "extract uris" in {
      "from a turtle resource in IRI format" in new IriExtractScope {
        val typeIri = unmarshaller invokePrivate iriFromTurtle("<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>", ParserState.Empty)
        typeIri must be equalTo "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"
      }
      "from a turtle resource in prefix:name format" in new IriExtractScope {
        val parserState = ParserState.Empty withPrefix Prefix("mo", "http://purl.org/ontology/mo/")
        val musicArtistIri = unmarshaller invokePrivate iriFromTurtle("mo:MusicArtist", parserState)
        musicArtistIri must be equalTo "http://purl.org/ontology/mo/MusicArtist"
      }
      "from a relative URI turtle resource" in new IriExtractScope {
        val parserState = ParserState.Empty withPrefix BasePrefix("http://purl.org/ontology/mo/")
        val musicArtistIri = unmarshaller invokePrivate iriFromTurtle("<MusicArtist>", parserState)
        musicArtistIri must be equalTo "http://purl.org/ontology/mo/MusicArtist"
      }
      "from the RDF standard abbrieviation'a'" in new IriExtractScope {
        val typeIri = unmarshaller invokePrivate iriFromTurtle("a", ParserState.Empty)
        typeIri must be equalTo "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"
      }
      "throwing an exception if a prefixed resource has no matching prefix" in new IriExtractScope {
        unmarshaller invokePrivate iriFromTurtle("mo:MusicArtist", ParserState.Empty) must throwA[TurtleParseException]
      }
      "throwing an exception if a resource is not in a recognised format" in new IriExtractScope {
        unmarshaller invokePrivate iriFromTurtle("moMusicArtist", ParserState.Empty) must throwA[TurtleParseException]
      }
    }

    "create resources" in {
      "from a standard resource" in new CreateResourcesScope {
        unmarshaller invokePrivate resourceFromTurtle("<urn:name:me>", ParserState.Empty) must be equalTo Resource("urn:name:me")
      }
      "from a labeled blank node" in new CreateResourcesScope {
        unmarshaller invokePrivate resourceFromTurtle("_:someone", ParserState.Empty) must be equalTo BlankNode("someone")
      }
      "from an unlabelled blank node" in new CreateResourcesScope {
        unmarshaller invokePrivate resourceFromTurtle("[]", ParserState.Empty) must be equalTo BlankNode("blank-1")
      }
      "from a second unlabelled blank node" in new CreateResourcesScope {
        val parserState = ParserState(blankNodes = Seq(BlankNode("blank-1")))
        unmarshaller invokePrivate resourceFromTurtle("[]", parserState) must be equalTo BlankNode("blank-2")
      }
    }

    "create literals" in {
      "from a turtle string without a language" in new CreateLiteralsScope {
        val literal = unmarshaller invokePrivate nodeFromTurtle("\"Justin Bieber\"", ParserState.Empty)
        literal must be equalTo StandardStringLiteral("Justin Bieber")
      }
      "from a turtle string with a language" in new CreateLiteralsScope {
        val literal = unmarshaller invokePrivate nodeFromTurtle("\"Justine Biebere\"@fr", ParserState.Empty)
        literal must be equalTo LanguageStringLiteral("Justine Biebere", "fr")
      }
      "from a turtle string with a custom IRI" in new CreateLiteralsScope {
        val literal = unmarshaller invokePrivate nodeFromTurtle("\"Justin Bieber\"^^<http://www.crazyencodings.co.uk/encoding>", ParserState.Empty)
        literal must be equalTo NonStandardStringLiteral("Justin Bieber", "http://www.crazyencodings.co.uk/encoding")
      }
      "from a turtle boolean" in new CreateLiteralsScope {
        val literal = unmarshaller invokePrivate nodeFromTurtle("false", ParserState.Empty)
        literal must be equalTo BooleanLiteral(false)
      }
      "from a turtle integer" in new CreateLiteralsScope {
        val literal = unmarshaller invokePrivate nodeFromTurtle("684", ParserState.Empty)
        literal must be equalTo IntegerLiteral(684)
      }
      "from an explicitly positive turtle integer" in new CreateLiteralsScope {
        val literal = unmarshaller invokePrivate nodeFromTurtle("+684", ParserState.Empty)
        literal must be equalTo IntegerLiteral(684)
      }
      "from a negative turtle integer" in new CreateLiteralsScope {
        val literal = unmarshaller invokePrivate nodeFromTurtle("-684", ParserState.Empty)
        literal must be equalTo IntegerLiteral(-684)
      }
      "from a turtle decimal" in new CreateLiteralsScope {
        val literal = unmarshaller invokePrivate nodeFromTurtle("6.84", ParserState.Empty)
        literal must be equalTo DecimalLiteral(6.84)
      }
      "from a negative turtle decimal" in new CreateLiteralsScope {
        val literal = unmarshaller invokePrivate nodeFromTurtle("-6.84", ParserState.Empty)
        literal must be equalTo DecimalLiteral(-6.84)
      }
    }

  }

}

trait RdrrTurtleUnmarshallerScope extends Scope with TestHelpers {
  val unmarshaller = RdrrTurtleUnmarshaller
}

trait IriExtractScope extends RdrrTurtleUnmarshallerScope with PrivateMethodTester {
  val iriFromTurtle = PrivateMethod[String]('iriFromTurtle)
}

trait CreateResourcesScope extends RdrrTurtleUnmarshallerScope with PrivateMethodTester {
  val resourceFromTurtle = PrivateMethod[RdfResource]('resourceFromTurtle)
}

trait CreateLiteralsScope extends RdrrTurtleUnmarshallerScope with PrivateMethodTester {
  val nodeFromTurtle = PrivateMethod[GraphNode]('nodeFromTurtle)
}

trait EntitiesFromLinesScope extends RdrrTurtleUnmarshallerScope with PrivateMethodTester {
  val entitiesFromLines = PrivateMethod[Stream[String]]('entitiesFromLines)
}

trait PartitionBlankNodeScopeScope extends RdrrTurtleUnmarshallerScope with PrivateMethodTester {
  val partitionBlankNodeScope = PrivateMethod[(Stream[String], Stream[String])]('partitionBlankNodeScope)
}

trait ConvertCollectionScope extends RdrrTurtleUnmarshallerScope with PrivateMethodTester {
  val convertCollectionToBlankNodes = PrivateMethod[Stream[String]]('convertCollectionToBlankNodes)
}


class RdrrParserStateSpec extends Specification {

  "The ParserState" should {

    "return the correct state when a standard prefix is added" in  {
      val musicOntologyPrefix = Prefix("mo", "http://purl.org/ontology/mo/")
      val initialState = ParserState(prefixes = Nil)
      val expectedState = ParserState(prefixes = Seq(musicOntologyPrefix))
      initialState withPrefix musicOntologyPrefix must be equalTo expectedState
    }

    "overwrite older prefix mappings if they have the same prefix" in {
      val musicOntologyPrefix = Prefix("mo", "http://purl.org/ontology/mo/")
      val wikipediaPrefix = Prefix("wiki", "http://en.wikipedia.org/wiki/")
      val kanzakiMusicOntologyPrefix = Prefix("mo", "http://www.kanzaki.com/ns/music#")

      val initialState = ParserState(prefixes = Seq(musicOntologyPrefix, wikipediaPrefix))
      val expectedState = ParserState(prefixes = Seq(wikipediaPrefix, kanzakiMusicOntologyPrefix))

      initialState withPrefix kanzakiMusicOntologyPrefix must be equalTo expectedState
    }

    "return the correct state when a base prefix is added" in {
      val musicOntologyBasePrefix = BasePrefix("http://purl.org/ontology/mo/")
      val initialState = ParserState(prefixes = Nil)
      val expectedState = ParserState(basePrefix = Some(musicOntologyBasePrefix))
      initialState withPrefix musicOntologyBasePrefix must be equalTo expectedState
    }

    "overwrite older base prefixes" in {
      val basePrefix1 = BasePrefix("http://base.prefix/1/")
      val basePrefix2 = BasePrefix("http://base.prefix/2/")
      val initialState = ParserState(basePrefix = Some(basePrefix1))
      val expectedState = ParserState(basePrefix = Some(basePrefix2))
      initialState withPrefix basePrefix2 must be equalTo expectedState
    }

    "overwrite partial triples" in {
      val initialState = ParserState.Empty.copy(partialTriple = Subject(BlankNode("blank-1")))
      initialState withPartial EmptyTriple must be equalTo ParserState.Empty
    }

    "generate blank nodes with unique labels" in {
      val initialState = ParserState(prefixes = Nil)
      val newBlankNode = initialState.nextBlankNode
      newBlankNode.label must be equalTo "blank-1"
    }

    "add nodes to blank nodes when appropriate" in {
      val initialState = ParserState(prefixes = Nil)

      val resourceNode = Resource("bieber")
      initialState withNode resourceNode  must be equalTo initialState
      val blankNode = BlankNode("label")
      initialState withNode blankNode must be equalTo ParserState(blankNodes = Seq(blankNode))
    }
  }

}
