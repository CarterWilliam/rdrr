package rdrr.immutable.marshallers

import rdrr.immutable._

import scala.io.Source

class RdrrTurtleMarshaller extends TurtleMarshaller {

  val AnotherObjectNext = ","
  val AnotherPredicateNext = ";"
  val AnotherSubjectNext = "."

  val RdfStandardResources: Map[String, String] = Map {
    "a" -> "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"
  }

  override def fromTurtle(turtle: String): Graph = fromTurtle(Source.fromString(turtle).getLines().toStream)

  def fromTurtle(lines: Stream[String]) = Graph(triplesFromEntities(entitiesFromLines(lines)))

  private[this] val Prefix = """^@prefix\s+(.*):\s*<(.*)>\s*\.$""".r
  private[this] def triplesFromEntities(entities: Stream[String],
                       prefixes: Map[String, String] = Map.empty,
                       partialTriple: PartialTriple = EmptyTriple): Stream[Triple] = entities match {

    case Prefix(prefix, uri) #:: rest =>
      // Add to prefixes and continue.
      triplesFromEntities(rest, prefixes + (prefix -> uri), partialTriple)

    case entity #:: rest => {
      partialTriple match {
        case EmptyTriple =>
          val unfinishedTriple = Subject(Resource(iriFromTurtleRepresentation(entity, prefixes)))
          triplesFromEntities(rest, prefixes, unfinishedTriple)

        case Subject(subject) =>
          val unfinishedTriple = SubjectAndPredicate(subject, Predicate(iriFromTurtleRepresentation(entity, prefixes)))
          triplesFromEntities(rest, prefixes, unfinishedTriple)

        case SubjectAndPredicate(subject, predicate) => {
          entity match {
            case AnotherObjectNext =>
              triplesFromEntities(rest, prefixes, SubjectAndPredicate(subject, predicate))
            case AnotherPredicateNext =>
              triplesFromEntities(rest, prefixes, Subject(subject))
            case AnotherSubjectNext =>
              triplesFromEntities(rest, prefixes, EmptyTriple)
            case resourceTurtle =>
              Triple(subject, predicate, nodeFromTurtle(resourceTurtle, prefixes)) #:: triplesFromEntities(rest, prefixes, partialTriple)
          }
        }
      }
    }

    case Stream.Empty => Stream.Empty // done
  }


  private[this] def entitiesFromLines(lines: Stream[String]): Stream[String] = {
    val PrefixLine = """^\s*(@(?:base|prefix)\s+.*\.)\s*$""".r
    val PunctuationEtc = """^\s*([,;.])\s*(.*)$""".r
    val StringLiteralEtc = """^\s*("[^"]*"[^\s;,.]*)\s*(.*)$""".r
    val ResourceEtc = """^\s*([^\s'"]*[^\s'",;.])\s*(.*)$""".r

    lines match {

      case line #:: moreLines if line.isEmpty =>
        entitiesFromLines(moreLines)

      case PrefixLine(prefixLine) #:: moreLines =>
        prefixLine #:: entitiesFromLines(moreLines)

      case PunctuationEtc(punctuation, etc) #:: moreLines =>
        punctuation #:: entitiesFromLines(etc #:: moreLines)

      case StringLiteralEtc(stringLiteral, etc) #:: moreLines =>
        stringLiteral #:: entitiesFromLines(etc #:: moreLines)

      case ResourceEtc(resource, etc) #:: moreLines =>
        resource #:: entitiesFromLines(etc #:: moreLines)

      case Stream.Empty =>
        Stream.Empty

      case unmatchedLine #:: rest =>
        throw new TurtleParseError(s"RDRR Turtle Marshaller could not parse the line: '$unmatchedLine'")
    }
  }

  private[this] def iriFromTurtleRepresentation(turtleRepresentation: String, prefixedIris: Map[String, String]): String = {
    val UriResource = "<(.*)>".r
    val PrefixedResource = "(.*):(.*)".r

    turtleRepresentation match {
      case rdfShorthand if RdfStandardResources.contains(rdfShorthand) => RdfStandardResources(rdfShorthand)
      case UriResource(uri) => uri
      case PrefixedResource(prefix, name) => prefixedIris.get(prefix).map(_ + name).getOrElse {
        throw new ParseException(s"Resource does not have given prefix defined in the document: $prefix")
      }
      case unmatched => throw new ParseException(s"turtle representation not in a form understood by the parser: $unmatched")
    }
  }

  private[this] def nodeFromTurtle(turtleRepresentation: String, prefixedIris: Map[String, String]): Node = {
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
        NonStandardStringLiteral(string, iriFromTurtleRepresentation(turtleResource, prefixedIris))
      case BooleanLiteralMatcher(boolean) => BooleanLiteral(boolean.toBoolean)
      case IntegerLiteralMatcher(integer) => IntegerLiteral(integer.toInt)
      case DecimalLiteralMatcher(decimal) => DecimalLiteral(decimal.toDouble)
      case _ => Resource(iriFromTurtleRepresentation(turtleRepresentation, prefixedIris))
    }
  }

  override def toTurtle(graph: Graph): String = graph.subjects.foldLeft("") { (out, subject) =>
    ???
  }

}

case class Prefix(prefix: String, uri: String)

sealed abstract class PartialTriple
object EmptyTriple extends PartialTriple
case class Subject(subject: Resource) extends PartialTriple
case class SubjectAndPredicate(subject: Resource, predicate: Predicate) extends PartialTriple

class ParseException(message: String) extends Exception(message: String)

