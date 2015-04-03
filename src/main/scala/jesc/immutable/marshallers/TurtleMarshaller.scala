package jesc.immutable.marshallers

import jesc.immutable._

import scala.io.Source

class TurtleMarshaller extends GraphMarshaller {

  val AnotherObjectNext = ","
  val AnotherPredicateNext = ";"
  val AnotherSubjectNext = "."

  val RdfStandardPredicates: Map[String, Predicate] = Map {
    "a" -> Predicate("http://www.w3.org/1999/02/22-rdf-syntax-ns#type")
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
          triplesFromLines(otherResources #:: rest, prefixes, Subject(resourceFromTurtle(nextResource, prefixes)))

        case Subject(subject) =>
          triplesFromLines(otherResources #:: rest, prefixes, SubjectAndPredicate(subject, predicateFromTurtle(nextResource, prefixes)))

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

  private[this] def fromTurtleRepresentation[T <: Node](turtleRepresentation: String, prefixes: Map[String, String])(apply: String => T): T = {
    val UriResource = "<(.*)>".r
    val PrefixedResource = "(.*):(.*)".r

    turtleRepresentation match {
      case UriResource(uri) => apply(uri)
      case PrefixedResource(prefix, name) => prefixes.collectFirst {
        case (px, uri) if prefix == px => apply(uri + name)
      }.getOrElse {
        throw new ParseException(s"No such prefix in the document: $prefix")
      }
      case unmatched => throw new ParseException(s"Turtle representation not recognised by parser: $unmatched")
    }
  }

  private[this] def resourceFromTurtle(turtleRepresentation: String, prefixes: Map[String, String]): Resource =
    fromTurtleRepresentation(turtleRepresentation, prefixes)(Resource)

  private[this] def predicateFromTurtle(turtleRepresentation: String, prefixes: Map[String, String]): Predicate =
    RdfStandardPredicates.getOrElse(turtleRepresentation, fromTurtleRepresentation(turtleRepresentation, prefixes)(Predicate))

  private[this] def nodeFromTurtle(turtleRepresentation: String, prefixes: Map[String, String]): Node = {
    val StringLiteralWithLanguage = "\"(.*)\"@(.*)".r
    val SimpleStringLiteral = "\"(.*)\"".r

    turtleRepresentation match {
      case StringLiteralWithLanguage(string, language) => StringLiteral(string, language)
      case SimpleStringLiteral(string) => StringLiteral(string)
      case _ => resourceFromTurtle(turtleRepresentation, prefixes)
    }
  }

  override def toTurtle(graph: Graph): String = ???

}

case class Prefix(prefix: String, uri: String)
abstract class UnfinishedTriple
object EmptyTriple extends UnfinishedTriple
case class Subject(subject: Resource) extends UnfinishedTriple
case class SubjectAndPredicate(subject: Resource, predicate: Predicate) extends UnfinishedTriple

class ParseException(message: String) extends Exception(message: String)

