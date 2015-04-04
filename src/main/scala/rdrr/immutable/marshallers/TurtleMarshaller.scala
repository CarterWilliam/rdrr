package rdrr.immutable.marshallers

import rdrr.immutable._

import scala.io.Source

class TurtleMarshaller extends GraphMarshaller {

  val AnotherObjectNext = ","
  val AnotherPredicateNext = ";"
  val AnotherSubjectNext = "."

  val RdfStandardResources: Map[String, String] = Map {
    "a" -> "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"
  }

  override def fromTurtle(turtle: String): Graph = fromTurtle(Source.fromString(turtle).getLines().toStream)

  def fromTurtle(lines: Stream[String]) = Graph(triplesFromLines(lines))

  private[this] val PrefixLine = """^@prefix (.*): (.*) .$""".r
  private[this] def triplesFromLines(lines: Stream[String],
                       prefixes: Map[String, String] = Map.empty,
                       unfinishedTriple: UnfinishedTriple = EmptyTriple): Stream[Triple] = lines match {

    case PrefixLine(prefix, uri) #:: rest =>
      // Add to prefixes and continue.
      triplesFromLines(rest, prefixes + (prefix -> uri), unfinishedTriple)

    case line #:: rest if line.isEmpty =>
      // Ignore and continue.
      triplesFromLines(rest, prefixes, unfinishedTriple)

    case line #:: rest => {
      // Get next resource in the line. Add triple if subject.
      val (nextResource, otherResources) = splitResourceString(line)
      unfinishedTriple match {
        case EmptyTriple =>
          val unfinishedTriple = Subject(Resource(iriFromTurtleRepresentation(nextResource, prefixes)))
          triplesFromLines(otherResources #:: rest, prefixes, unfinishedTriple)

        case Subject(subject) => {
          val unfinishedTriple = SubjectAndPredicate(subject, Predicate(iriFromTurtleRepresentation(nextResource, prefixes)))
          triplesFromLines(otherResources #:: rest, prefixes, unfinishedTriple)
        }

        case SubjectAndPredicate(subject, predicate) => {
          nextResource match {
            case AnotherObjectNext =>
              triplesFromLines(otherResources #:: rest, prefixes, SubjectAndPredicate(subject, predicate))
            case AnotherPredicateNext =>
              triplesFromLines(otherResources #:: rest, prefixes, Subject(subject))
            case AnotherSubjectNext =>
              triplesFromLines(otherResources #:: rest, prefixes, EmptyTriple)
            case resourceTurtle =>
              Triple(subject, predicate, nodeFromTurtle(resourceTurtle, prefixes)) #:: triplesFromLines(otherResources #:: rest, prefixes, unfinishedTriple)
          }
        }
      }
    }

    case Stream.Empty => Stream.Empty // done
  }

  private[this] def splitResourceString(resourceString: String): (String, String) = {
    val StringLiteralEtc = "^(\".*\") (.*)$".r
    val StringLiteralWithLanguageEtc = "^(\".*\"@\\S*) (.*)$".r
    val ResourceEtc = "^(\\S*) (.*)$".r
    
    resourceString.trim match {
      case StringLiteralEtc(stringLiteral, etc) => (stringLiteral, etc)
      case StringLiteralWithLanguageEtc(stringLiteral, etc) => (stringLiteral, etc)
      case ResourceEtc(resource, etc) => (resource, etc)
      case lastResource => (lastResource, "")
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
    val StringLiteralWithLanguage = "\"(.*)\"@(.*)".r
    val SimpleStringLiteral = "\"(.*)\"".r
    val StringLiteralWithCustomIRI = "\"(.*)\"\\^\\^(.*)".r

    turtleRepresentation match {
      case StringLiteralWithLanguage(string, language) => LanguageStringLiteral(string, language)
      case SimpleStringLiteral(string) => StringLiteral(string)
      case StringLiteralWithCustomIRI(string, turtleResource) =>
        NonStandardStringLiteral(string, iriFromTurtleRepresentation(turtleResource, prefixedIris))
      case _ => Resource(iriFromTurtleRepresentation(turtleRepresentation, prefixedIris))
    }
  }

  override def toTurtle(graph: Graph): String = graph.subjects.foldLeft("") { (out, subject) =>
    out + s"<${subject.uri}>"
  }

}

case class Prefix(prefix: String, uri: String)
abstract class UnfinishedTriple
object EmptyTriple extends UnfinishedTriple
case class Subject(subject: Resource) extends UnfinishedTriple
case class SubjectAndPredicate(subject: Resource, predicate: Predicate) extends UnfinishedTriple

class ParseException(message: String) extends Exception(message: String)

