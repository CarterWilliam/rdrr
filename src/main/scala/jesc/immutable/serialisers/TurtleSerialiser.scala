package jesc.immutable.serialisers

import jesc.immutable.{Graph, Node, Predicate, Resource, Triple}

import scala.io.Source

class TurtleSerialiser extends GraphSerialiser {

  override def fromTurtle(turtle: String): Graph = fromTurtle(Source.fromString(turtle).getLines().toStream)

  def fromTurtle(lines: Stream[String]) = {
    val PrefixLine = """^@prefix (.*): (.*) .$""".r

    def triplesFromLines(lines: Stream[String],
                         prefixes: Map[String, String] = Map.empty,
                         unfinishedTriple: UnfinishedTriple = Empty): Stream[Triple] = {

      println("triplesFromLines")
      println(lines)
      println(prefixes)

      lines match {
        case PrefixLine(prefix, uri) #:: rest => {
          println(s"Prefix found: $prefix, $uri")
          triplesFromLines(rest, prefixes + (prefix -> uri), unfinishedTriple)
        }
        case line #:: rest if line.isEmpty => {
          println("empty line. continue...")
          triplesFromLines(rest, prefixes, unfinishedTriple)
        }
        case line #:: rest => {
          val (nextResource, otherResources) = line.trim.span(!_.isWhitespace)
          unfinishedTriple match {
            case Empty => 
              triplesFromLines(otherResources #:: rest, prefixes, Subject(resourceFromTurtle(nextResource, prefixes)))
            case Subject(subject) => 
              triplesFromLines(otherResources #:: rest, prefixes, SubjectAndPredicate(subject, predicateFromTurtle(nextResource, prefixes)))
            case SubjectAndPredicate(subject, predicate) => {
              nextResource match {
                case UriResource(_) => {
                  println("Found a triple!")
                  println(subject)
                  println(predicate)
                  Triple(subject, predicate, resourceFromTurtle(nextResource, prefixes)) #:: triplesFromLines(otherResources #:: rest, prefixes, unfinishedTriple)
                }
                case PrefixedResource(_, _) => {
                  println("Found a triple!")
                  println(subject)
                  println(predicate)
                  Triple(subject, predicate, resourceFromTurtle(nextResource, prefixes)) #:: triplesFromLines(otherResources #:: rest, prefixes, unfinishedTriple)
                }
                case AnotherObjectNext => {
                  println("Found ','")
                  triplesFromLines(otherResources #:: rest, prefixes, SubjectAndPredicate(subject, predicate))
                }
                case AnotherPredicateNext => {
                  println("Found ';'")
                  triplesFromLines(otherResources #:: rest, prefixes, Subject(subject))
                }
                case AnotherSubjectNext => {
                  println("Found '.'")
                  triplesFromLines(otherResources #:: rest, prefixes, Empty)
                }
              }
            }
          }
        }
        case Stream.Empty => Stream.Empty
      }
    }
    Graph(triplesFromLines(lines))
  }

  val UriResource = "<(.*)>".r
  val PrefixedResource = "(.*):(.*)".r
  val AnotherObjectNext = ","
  val AnotherPredicateNext = ";"
  val AnotherSubjectNext = "."
  private def fromTurtleRepresentation[T <: Node](turtleRepresentation: String, prefixes: Map[String, String])(apply: String => T): T =
    turtleRepresentation match {
      case UriResource(uri) => apply(uri)
      case PrefixedResource(prefix, name) => prefixes.collectFirst {
        case (px, uri) if prefix == px => apply(uri + name)
      }.getOrElse {
        throw new ParseException(s"No such prefix in the document: $prefix")
      }
      case unmatched => throw new ParseException(s"Turtle representation not recognised by parser: $unmatched")
    }

  private def resourceFromTurtle(turtleRepresentation: String, prefixes: Map[String, String]): Resource =
    fromTurtleRepresentation(turtleRepresentation, prefixes)(Resource)

  private def predicateFromTurtle(turtleRepresentation: String, prefixes: Map[String, String]): Predicate =
    RdfStandardPredicates.getOrElse(turtleRepresentation, fromTurtleRepresentation(turtleRepresentation, prefixes)(Predicate))

  val RdfStandardPredicates: Map[String, Predicate] = Map {
    "a" -> Predicate("http://www.w3.org/1999/02/22-rdf-syntax-ns#type")
  }


  override def toTurtle(graph: Graph): String = ???
}

case class Prefix(prefix: String, uri: String)
abstract class UnfinishedTriple
object Empty extends UnfinishedTriple
case class Subject(subject: Resource) extends UnfinishedTriple
case class SubjectAndPredicate(subject: Resource, predicate: Predicate) extends UnfinishedTriple

class ParseException(message: String) extends Exception(message: String)

