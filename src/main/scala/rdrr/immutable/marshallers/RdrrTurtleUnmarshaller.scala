package rdrr.immutable.marshallers

import rdrr.immutable._

import scala.io.Source

object RdrrTurtleUnmarshaller extends TurtleUnmarshaller {

  val RdfStandard: Map[String, String] =
    Map("a" -> "http://www.w3.org/1999/02/22-rdf-syntax-ns#type")

  override def fromTurtle(turtle: String): Graph = fromTurtle(Source.fromString(turtle).getLines().toStream)
  def fromTurtle(lines: Stream[String]) = Graph(triplesFromEntities(entitiesFromLines(lines)))


  private[this] def entitiesFromLines(lines: Stream[String]): Stream[String] = {
    val EmptyLine = """^\s*$""".r
    val CommentLine = """^\s*#.*$""".r
    val PrefixLine = """^\s*(@(?:base|BASE|prefix|PREFIX)\s+.*\.)\s*$""".r
    val UnlabeledBlankNodeEtc = """^\s*\[\s*\]\s*(.*)$""".r
    val EntityEtc = """^\s*([^\s'"]*[^\s'";,.])\s*(.*)$""".r // Resources, Literals, Labeled Blank Nodes
    val StringLiteralEtc = """^\s*(("|').*?\2[^\s;,.]*)\s*(.*)$""".r // also matches Triple quoted string literals!
    val TripleQuotedStringLiteralEtc = "\\s*((\"\"\"|''')(?s).*?\\2[^\\s;,.]*)\\s*(.*)".r
    val MultilineStringLiteralBegin = "^\\s*((\"\"\"|''').*)".r
    val PunctuationEtc = """^\s*([;,.])\s*(.*)$""".r

    lines match {

      case EmptyLine() #:: moreLines => entitiesFromLines(moreLines)

      case CommentLine() #:: moreLines => entitiesFromLines(moreLines)

      case PrefixLine(prefixLine) #:: moreLines =>
        prefixLine #:: entitiesFromLines(moreLines)

      case UnlabeledBlankNodeEtc(etc) #:: moreLines =>
        "[]" #:: entitiesFromLines(etc #:: moreLines)

      case EntityEtc(resource, etc) #:: moreLines =>
        resource #:: entitiesFromLines(etc #:: moreLines)

      case TripleQuotedStringLiteralEtc(stringLiteral, quoteType, etc) #:: moreLines =>
        stringLiteral #:: entitiesFromLines(etc #:: moreLines)

      case MultilineStringLiteralBegin(stringStart, quoteType) #:: nextLine #:: moreLines =>
        entitiesFromLines(s"$stringStart\n$nextLine" #:: moreLines)

      case StringLiteralEtc(stringLiteral, quoteType, etc) #:: moreLines =>
        stringLiteral #:: entitiesFromLines(etc #:: moreLines)

      case PunctuationEtc(punctuation, etc) #:: moreLines =>
        punctuation #:: entitiesFromLines(etc #:: moreLines)

      case Stream.Empty => Stream.Empty

      case unmatchedLine #:: rest =>
        throw new TurtleParseException(s"RDRR Turtle Marshaller could not parse the line: '$unmatchedLine'")
    }
  }

  case class ParserState(basePrefix: Option[BasePrefix] = None,
                         prefixes: Seq[Prefix] = Nil,
                         blankNodes: Seq[BlankNode] = Nil,
                         partialTriple: PartialTriple = EmptyTriple) {

    def withPrefix (prefix: RdfPrefix): ParserState = prefix match {
      case basePrefix: BasePrefix =>
        copy(basePrefix = Some(basePrefix))
      case standardPrefix: Prefix =>
        val updatePrefixes = prefixes.filter(_.prefix != standardPrefix.prefix) :+ standardPrefix
        copy(prefixes = updatePrefixes)
    }

    def withPartial(partial: PartialTriple) = copy(partialTriple = partial)

    def withNode(node: GraphNode) = node match {
      case blankNode: BlankNode => copy(blankNodes = blankNode +:blankNodes)
      case _ => this
    }

    def nextBlankNode: BlankNode = {

      def iteration(n: Int): BlankNode = {
        if (blankNodes contains BlankNode("blank-" + n))
          iteration(n+1)
        else
          BlankNode("blank-" + n)
      }

      iteration(1)
    }

  }

  object ParserState {
    val Empty = ParserState(None, Nil, Nil, EmptyTriple)
  }

  sealed abstract class PartialTriple
  object EmptyTriple extends PartialTriple
  case class Subject(subject: RdfResource) extends PartialTriple
  case class SubjectAndPredicate(subject: RdfResource, predicate: Predicate) extends PartialTriple

  private[this] def triplesFromEntities(entities: Stream[String],
                                        parserState: ParserState = ParserState.Empty): Stream[Triple] = {

    val BasePrefixExtractor = """^@(?:base|BASE)\s+<(.*)>\s*\.$""".r
    val PrefixExtractor = """^@(?:prefix|PREFIX)\s+(.*):\s*<(.*)>\s*\.$""".r
    val AnotherPredicateNext = ";"
    val AnotherObjectNext = ","
    val AnotherSubjectNext = "."

    entities match {

      case BasePrefixExtractor(path) #:: rest =>
        triplesFromEntities(rest, parserState withPrefix BasePrefix(path))

      case PrefixExtractor(prefix, uri) #:: rest =>
        triplesFromEntities(rest, parserState withPrefix Prefix(prefix, uri))

      case entity #:: rest => parserState.partialTriple match {

        case EmptyTriple =>
          val subjectNode = resourceFromTurtle(entity, parserState)
          val newState = parserState withNode subjectNode withPartial Subject(subjectNode)
          triplesFromEntities(rest, newState)

        case Subject(subject) =>
          val subjectPredicatePartial = SubjectAndPredicate(subject, Predicate(iriFromTurtle(entity, parserState)))
          triplesFromEntities(rest, parserState withPartial subjectPredicatePartial)

        case SubjectAndPredicate(subject, predicate) => {
          entity match {
            case AnotherObjectNext =>
              triplesFromEntities(rest, parserState withPartial SubjectAndPredicate(subject, predicate))
            case AnotherPredicateNext =>
              triplesFromEntities(rest, parserState withPartial Subject(subject))
            case AnotherSubjectNext =>
              triplesFromEntities(rest, parserState withPartial EmptyTriple)
            case resourceTurtle =>
              val objectNode = nodeFromTurtle(resourceTurtle, parserState)
              val newState = parserState withNode objectNode
              Triple(subject, predicate, objectNode) #:: triplesFromEntities(rest, newState)
          }
        }
      }

      case Stream.Empty => Stream.Empty // done
    }
  }


  private[this] def iriFromTurtle(turtleRepresentation: String, parserState: ParserState): String = {
    val UriResource = "^<([^:/?#]+:.*)>$".r // URI with scheme - RFC 3986
    val RelativeUriResource = "<(.*)>".r
    val PrefixedResource = "(.*):(.*)".r

    def withPrefix(resource: String)(prefix: RdfPrefix) = prefix.path + resource

    turtleRepresentation match {

      case rdfStandard if RdfStandard.contains(rdfStandard) => RdfStandard(rdfStandard)

      case UriResource(uri) => uri

      case RelativeUriResource(resource) if parserState.basePrefix.isDefined =>
        parserState.basePrefix.map(withPrefix(resource)).get

      case PrefixedResource(prefix, name) => parserState.prefixes.find(_.prefix == prefix).map(withPrefix(name)).getOrElse {
        throw new TurtleParseException(s"Resource does not have a prefix with key $prefix in scope")
      }

      case unmatched => throw new TurtleParseException(s"turtle representation not in a form understood by the parser: $unmatched")
    }
  }

  private[this] def resourceFromTurtle(turtleRepresentation: String, parserState: ParserState): RdfResource = {
    val LabeledBlankNode = """^_:([^\s]*)$""".r
    val UnlabeledBlankNode = "[]"

    turtleRepresentation match {
      case LabeledBlankNode(label) => BlankNode(label)
      case UnlabeledBlankNode => parserState.nextBlankNode
      case resourceString => Resource(iriFromTurtle(resourceString, parserState))
    }
  }

  private[this] def nodeFromTurtle(turtleRepresentation: String, parserState: ParserState): GraphNode = {
    val StringLiteralWithLanguageMatcher = """^"(.*)"@(.*)$""".r
    val SimpleStringLiteralMatcher = """^"(.*)"$""".r
    val StringLiteralWithCustomIRIMatcher = """^"(.*)"\^\^(.*)$""".r
    val BooleanLiteralMatcher = """^(true|false)$""".r
    val IntegerLiteralMatcher = """^\+?(-?[0-9]+)$""".r
    val DecimalLiteralMatcher = """^\+?(-?[0-9]*\.[0-9]+)$""".r

    turtleRepresentation match {
      case StringLiteralWithLanguageMatcher(string, language) => LanguageStringLiteral(string, language)
      case SimpleStringLiteralMatcher(string) => StandardStringLiteral(string)
      case StringLiteralWithCustomIRIMatcher(string, turtleResource) =>
        NonStandardStringLiteral(string, iriFromTurtle(turtleResource, parserState))
      case BooleanLiteralMatcher(boolean) => BooleanLiteral(boolean.toBoolean)
      case IntegerLiteralMatcher(integer) => IntegerLiteral(integer.toInt)
      case DecimalLiteralMatcher(decimal) => DecimalLiteral(decimal.toDouble)
      case resourceString => resourceFromTurtle(resourceString, parserState)
    }
  }

}
