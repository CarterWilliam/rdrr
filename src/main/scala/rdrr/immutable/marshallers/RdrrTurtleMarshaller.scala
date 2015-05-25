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
    val EntityEtc = """^\s*([^\s'"]*[^\s'";,.])\s*(.*)$""".r
    val StringLiteralEtc = """^\s*(("|').*?\2[^\s;,.]*)\s*(.*)$""".r // also matches Triple quoted string literals!
    val TripleQuotedStringLiteralEtc = "\\s*((\"\"\"|''')(?s).*?\\2[^\\s;,.]*)\\s*(.*)".r
    val MultilineStringLiteralBegin = "^\\s*((\"\"\"|''').*)".r
    val PunctuationEtc = """^\s*([;,.])\s*(.*)$""".r

    lines match {

      case EmptyLine() #:: moreLines => entitiesFromLines(moreLines)

      case CommentLine() #:: moreLines => entitiesFromLines(moreLines)

      case PrefixLine(prefixLine) #:: moreLines =>
        prefixLine #:: entitiesFromLines(moreLines)

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

  case class ParserState(prefixes: Seq[Prefix], basePrefix: Option[BasePrefix], partialTriple: PartialTriple) {
    def + (prefix: RdfPrefix): ParserState = prefix match {
      case basePrefix: BasePrefix =>
        copy(basePrefix = Some(basePrefix))
      case standardPrefix: Prefix =>
        val updatePrefixes = prefixes.filter(_.prefix != standardPrefix.prefix) :+ standardPrefix
        copy(prefixes = updatePrefixes)
    }
  }

  object ParserState {
    val Empty = ParserState(Nil, None, EmptyTriple)
  }

  sealed abstract class PartialTriple
  object EmptyTriple extends PartialTriple
  case class Subject(subject: Resource) extends PartialTriple
  case class SubjectAndPredicate(subject: Resource, predicate: Predicate) extends PartialTriple

  private[this] def triplesFromEntities(entities: Stream[String],
                                        parserState: ParserState = ParserState.Empty): Stream[Triple] = {

    val BasePrefixExtractor = """^@(?:base|BASE)\s+<(.*)>\s*\.$""".r
    val PrefixExtractor = """^@(?:prefix|PREFIX)\s+(.*):\s*<(.*)>\s*\.$""".r
    val AnotherPredicateNext = ";"
    val AnotherObjectNext = ","
    val AnotherSubjectNext = "."

    entities match {

      case BasePrefixExtractor(path) #:: rest =>
        triplesFromEntities(rest, parserState + BasePrefix(path))

      case PrefixExtractor(prefix, uri) #:: rest =>
        triplesFromEntities(rest, parserState + Prefix(prefix, uri))

      case entity #:: rest => parserState.partialTriple match {

        case EmptyTriple =>
            val subjectPartial = Subject(Resource(iriFromTurtle(entity, parserState)))
            triplesFromEntities(rest, parserState.copy(partialTriple = subjectPartial))

        case Subject(subject) =>
          val subjectPredicatePartial = SubjectAndPredicate(subject, Predicate(iriFromTurtle(entity, parserState)))
          triplesFromEntities(rest, parserState.copy(partialTriple = subjectPredicatePartial))

        case SubjectAndPredicate(subject, predicate) => {
          entity match {
            case AnotherObjectNext =>
              triplesFromEntities(rest, parserState.copy(partialTriple = SubjectAndPredicate(subject, predicate)))
            case AnotherPredicateNext =>
              triplesFromEntities(rest, parserState.copy(partialTriple = Subject(subject)))
            case AnotherSubjectNext =>
              triplesFromEntities(rest, parserState.copy(partialTriple = EmptyTriple))
            case resourceTurtle =>
              Triple(subject, predicate, nodeFromTurtle(resourceTurtle, parserState)) #:: triplesFromEntities(rest, parserState)
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
      case _ => Resource(iriFromTurtle(turtleRepresentation, parserState))
    }
  }

}


object RdrrTurtleMarshaller extends TurtleMarshaller {

  val RdfStandard: Map[String, String] =
    Map ("http://www.w3.org/1999/02/22-rdf-syntax-ns#type" -> "a")

  def toVerboseTurtle(graph: Graph): String = graph.foldLeft(new StringBuilder) { (out, triple) =>
    out ++= s"""${asTurtle(triple.subject)} ${asTurtle(triple.predicate)} ${asTurtle(triple.`object`)} .\n"""
  }.toString()

  override def toTurtle(graph: Graph): String = {

    graph match {
      case Graph.Empty => ""
      case singleStatement +: Nil =>
        s"${asTurtle(singleStatement.subject)} ${asTurtle(singleStatement.predicate)} ${asTurtle(singleStatement.`object`)} .\n"
      case firstStatement +: moreStatements => {

        val firstTripleToTurtle = new StringBuilder (
          s"${asTurtle(firstStatement.subject)} ${asTurtle(firstStatement.predicate)} ${asTurtle(firstStatement.`object`)}" )
        val consequtiveTriples = graph.sliding(2).toStream

        def buildTurtleIteration(output: StringBuilder, consecutiveTriples: Seq[Seq[Triple]]): String = consecutiveTriples match {

          case Seq(alreadyAdded, toBeAdded) #:: more if subjectsAndPredicatesEqual(alreadyAdded, toBeAdded) =>
            output ++= s" , ${asTurtle(toBeAdded.`object`)}"
            buildTurtleIteration(output, more)

          case Seq(alreadyAdded, toBeAdded) #:: more if subjectsEqual(alreadyAdded, toBeAdded) =>
            output ++= s" ;\n\t${asTurtle(toBeAdded.predicate)} ${asTurtle(toBeAdded.`object`)}"
            buildTurtleIteration(output, more)

          case Nil =>
            (output ++= ".\n").toString()
        }

        def subjectsEqual(first: Triple, second: Triple) =
          first.subject == second.subject
        def subjectsAndPredicatesEqual(first: Triple, second: Triple) =
          subjectsEqual(first, second) && first.predicate == second.predicate

        buildTurtleIteration(firstTripleToTurtle, consequtiveTriples)
      }
    }
  }

  private[this] def asUriTurtle(rdfEntity: RdfEntity{ def uri: String }) =
    s"""<${rdfEntity.uri}>"""
  private[this] def asStandardStringLiteralTurtle(stringLiteral: StringLiteral) =
    s""""${stringLiteral.value}""""
  private[this] def asNonStandardStringLiteralTurtle(literal: NonStandardStringLiteral) =
    s"""${asStandardStringLiteralTurtle(literal)}^^${asUriTurtle(literal.datatype)}"""
  private[this] def asLanguageStringLiteralTurtle(literal: LanguageStringLiteral) =
    s""""${literal.value}"@${literal.language}"""
  private[this] def asLiteralTurtle(literal: Literal) =
    s"""${literal.value}^^${asUriTurtle(literal.datatype)}"""


  private[this] def asTurtle(entity: RdfEntity): String = entity match {
    case resource: Resource => asUriTurtle(resource)
    case predicate: Predicate => RdfStandard.getOrElse(predicate.uri, asUriTurtle(predicate))
    case string: StandardStringLiteral => asStandardStringLiteralTurtle(string)
    case nonStandardString: NonStandardStringLiteral => asNonStandardStringLiteralTurtle(nonStandardString)
    case languageString: LanguageStringLiteral => asLanguageStringLiteralTurtle(languageString)
    case otherLiteral: Literal => asLiteralTurtle(otherLiteral)
  }

}