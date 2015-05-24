package rdrr.immutable.marshallers

import rdrr.immutable._

import scala.io.Source

object RdrrTurtleUnmarshaller extends TurtleUnmarshaller {

  val RdfStandardResources: Map[String, String] = Map {
    "a" -> "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"
  }

  override def fromTurtle(turtle: String): Graph = fromTurtle(Source.fromString(turtle).getLines().toStream)
  def fromTurtle(lines: Stream[String]) = Graph(triplesFromEntities(entitiesFromLines(lines)))


  private[this] def entitiesFromLines(lines: Stream[String]): Stream[String] = {
    val EmptyLine = """^\s*$""".r
    val PrefixLine = """^\s*(@(?:base|BASE|prefix|PREFIX)\s+.*\.)\s*$""".r
    val EntityEtc = """^\s*([^\s'"]*[^\s'";,.])\s*(.*)$""".r
    val StringLiteralEtc = """^\s*(("|').*?\2[^\s;,.]*)\s*(.*)$""".r // also matches Triple quoted string literals!
    val TripleQuotedStringLiteralEtc = "\\s*((\"\"\"|''')(?s).*?\\2[^\\s;,.]*)\\s*(.*)".r
    val MultilineStringLiteralBegin = "^\\s*((\"\"\"|''').*)".r
    val PunctuationEtc = """^\s*([;,.])\s*(.*)$""".r

    lines match {

      case EmptyLine() #:: moreLines =>
        entitiesFromLines(moreLines)

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

      case entity #:: rest => {
        parserState.partialTriple match {
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
      }

      case Stream.Empty => Stream.Empty // done
    }
  }


  private[this] def iriFromTurtle(turtleRepresentation: String, parserState: ParserState): String = {
    val UriResource = "<(.*)>".r
    val PrefixedResource = "(.*):(.*)".r

    turtleRepresentation match {
      case rdfShorthand if RdfStandardResources.contains(rdfShorthand) => RdfStandardResources(rdfShorthand)
      case UriResource(uri) => uri
      case PrefixedResource(prefix, name) => parserState.prefixes.find(_.prefix == prefix).map(_.path + name).getOrElse {
        throw new TurtleParseException(s"Resource does not have a prefix with key $prefix in scope")
      }
      case unmatched => throw new TurtleParseException(s"turtle representation not in a form understood by the parser: $unmatched")
    }
  }

  private[this] def nodeFromTurtle(turtleRepresentation: String, parserState: ParserState): Node = {
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

sealed trait RdfPrefix {
  def path: String
}
case class Prefix(prefix: String, path: String) extends RdfPrefix
case class BasePrefix(path: String) extends RdfPrefix
