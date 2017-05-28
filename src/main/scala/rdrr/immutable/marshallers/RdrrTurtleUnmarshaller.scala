package rdrr.immutable.marshallers

import rdrr.immutable._
import rdrr.util.RandomStrings

import scala.io.Source

class RdrrTurtleUnmarshaller extends TurtleUnmarshaller with RandomStrings {

  override def fromTurtle(turtle: String): Graph = fromTurtle(Source.fromString(turtle).getLines().toStream)

  private def fromTurtle(lines: Stream[String]) = {
    val entities = entitiesFromLines(lines)
    val rewrittenEntities = expandBlankNodes { expandCollections { entities } }
    Graph(triplesFromEntities(rewrittenEntities))
  }

  val EmptyLine = """^\s*$""".r
  val CommentLine = """^\s*#.*$""".r
  val PrefixLine = """^((?:@base|@prefix|BASE|PREFIX).*)\s*$""".r
  val ScopeChangeEtc = """\s*(\[|\]|\(|\))\s*(.*)""".r
  val EntityEtc = """^\s*([^\s'"]*[^\s'";,.\]\)])\s*(.*)$""".r // Resources, Literals, labeled Blank Nodes
  val StringLiteralEtc = """^\s*(("|').*?\2[^\s;,.]*)\s*(.*)$""".r // also matches Triple quoted string literals!
  val TripleQuotedStringLiteralEtc = "\\s*((\"\"\"|''')(?s).*?\\2[^\\s;,.]*)\\s*(.*)".r
  val MultilineStringLiteralBegin = "^\\s*((\"\"\"|''').*)".r
  val PunctuationEtc = """^\s*([;,.])\s*(.*)$""".r

  private def entitiesFromLines(lines: Stream[String]): Stream[String] = {

    lines match {

      case EmptyLine() #:: moreLines => entitiesFromLines(moreLines)

      case CommentLine() #:: moreLines => entitiesFromLines(moreLines)

      case PrefixLine(prefixLine) #:: moreLines =>
        prefixLine #:: entitiesFromLines(moreLines)

      case ScopeChangeEtc(scopeCharacter, etc) #:: moreLines =>
        scopeCharacter #:: entitiesFromLines(etc #:: moreLines)

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

  private def expandCollections(entities: Stream[String]): Stream[String] = entities match {
    case CollectionStart #:: more =>
      val (collectionScope, mainScope) = partitionCollectionScope(more)
      expandCollections { convertCollectionToBlankNodes(collectionScope) #::: mainScope }
    case entity #:: more =>
      entity #:: expandCollections(more)
    case Stream.Empty => Stream.Empty
  }

  val UnlabeledBlankNodeStart = "["
  val UnlabeledBlankNodeEnd = "]"
  private def generateBlankNodeId() = randomAlpha(4)
  private def generateBlankNode(unavailable: Seq[String]): String = {
    val newBlankNode = "_:blank-" + generateBlankNodeId()
    if (unavailable.contains(newBlankNode)) generateBlankNode(unavailable) else newBlankNode
  }
  def expandBlankNodes(entities: Stream[String]): Stream[String] = {

    def expandBlankNodesIteration(entities: Stream[String], alreadySeen: Seq[String]): Stream[String] = entities match {
      case UnlabeledBlankNodeStart #:: UnlabeledBlankNodeEnd #:: more =>
        val newBlankNode = generateBlankNode(alreadySeen ++ entities)
        newBlankNode #:: expandBlankNodesIteration(more, newBlankNode +: alreadySeen)

      case UnlabeledBlankNodeStart #:: more =>
        val blankNode = generateBlankNode(alreadySeen ++ entities)
        val (blankNodeScope, mainScope) = partitionBlankNodeScope(more)
        val rewrittenMainScope = mainScope #::: blankNode #:: blankNodeScope :+ "."
        blankNode #:: expandBlankNodesIteration(rewrittenMainScope, blankNode +: alreadySeen)

      case entity #:: more =>
        entity #:: expandBlankNodesIteration(more, entity +: alreadySeen)

      case Stream.Empty => Stream.Empty
    }

    expandBlankNodesIteration(entities, Nil)
  }


  val BasePrefixExtractor = """^(?:@base|BASE)\s+<(.*)>\s*\.?$""".r
  val PrefixExtractor = """^(?:@prefix|PREFIX)\s+(.*):\s*<(.*)>\s*\.?$""".r

  val AnotherPredicateNext = ";"
  val AnotherObjectNext = ","
  val AnotherSubjectNext = "."

  val BlankNodeWithNestedTriplesStart = "["
  val BlankNodeWithNestedTriplesEnd = "]"

  private def partitionBlankNodeScope(entities: Stream[String], from: Int = 0): (Stream[String], Stream[String]) =
    partitionByScope(BlankNodeWithNestedTriplesStart, BlankNodeWithNestedTriplesEnd)(entities, from)

  val CollectionStart = "("
  val CollectionEnd = ")"
  
  private def partitionCollectionScope(entities: Stream[String], from: Int = 0): (Stream[String], Stream[String]) =
    partitionByScope(CollectionStart, CollectionEnd)(entities, from)

  private def convertCollectionToBlankNodes(collectionEntities: Stream[String]): Stream[String] = collectionEntities match {
    case CollectionStart #:: tail =>
      val (collectionScope, rest) = partitionCollectionScope(tail)
      Stream("[", "rdf:first", CollectionStart) #::: collectionScope #::: Stream(CollectionEnd, ";", "rdf:rest") #:::
        convertCollectionToBlankNodes(rest) #::: Stream("]")

    case BlankNodeWithNestedTriplesStart #:: tail =>
      val (collectionScope, rest) = partitionBlankNodeScope(tail)
      Stream("[", "rdf:first", BlankNodeWithNestedTriplesStart) #::: collectionScope #::: Stream(BlankNodeWithNestedTriplesEnd, ";", "rdf:rest") #:::
        convertCollectionToBlankNodes(rest) #::: Stream("]")

    case head #:: tail =>
      Stream("[", "rdf:first", head, ";", "rdf:rest") #::: convertCollectionToBlankNodes(tail) #::: Stream("]")

    case Stream.Empty => Stream("rdf:nil")
  }

  private def scopeIsClosed(open: String, close: String)(scope: Seq[String]) =
    scope.count(_ == open) == scope.count(_ == close)

  private def partitionByScope(open: String, close: String)(entities: Stream[String], from: Int = 0): (Stream[String], Stream[String]) =
  
    entities.indexOf(close, from) match {
      case -1 => throw new TurtleParseException("Entered blank node scope that did not terminate")
      case potentialScopeEndIndex =>
        entities.splitAt(potentialScopeEndIndex) match {
          case (blankNodeScope, closePlusMainScope) if scopeIsClosed(open, close)(blankNodeScope) =>
            (blankNodeScope, closePlusMainScope.tail)
          case _ =>
            partitionByScope(open,close)(entities, potentialScopeEndIndex + 1)
        }
    }

  private def triplesFromEntities(entities: Stream[String],
                                  parserState: ParserState = ParserState.Empty): Stream[Triple] = {

    entities match {

      case BasePrefixExtractor(path) #:: rest =>
        triplesFromEntities(rest, parserState withPrefix BasePrefix(path))

      case PrefixExtractor(prefix, uri) #:: rest =>
        triplesFromEntities(rest, parserState withPrefix Prefix(prefix, uri))

      case entity #:: rest => parserState.partialTriple match {

        case EmptyTriple =>
          val subjectNode = resourceFromTurtle(entity, parserState)
          triplesFromEntities(rest, parserState withPartial Subject(subjectNode))

        case Subject(subject) =>
          val subjectPredicatePartial = SubjectAndPredicate(subject, Resource(iriFromTurtle(entity, parserState)))
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
              Triple(subject, predicate, objectNode) #:: triplesFromEntities(rest, parserState)
          }
        }
      }

      case Stream.Empty => Stream.Empty // done
    }
  }


  val UriResource = "^<([^:/?#]+:.*)>$".r // URI with scheme - RFC 3986
  val RelativeUriResource = "<(.*)>".r
  val PrefixedResource = "(.*):(.*)".r

  private def iriFromTurtle(turtleRepresentation: String, parserState: ParserState): String = {

    def withPrefix(resource: String)(prefix: RdfPrefix) = prefix.path + resource

    turtleRepresentation match {

      case rdfStandard if RdfStandard.abrieviations.contains(rdfStandard) => RdfStandard.abrieviations(rdfStandard)

      case UriResource(uri) => uri

      case RelativeUriResource(resource) if parserState.basePrefix.isDefined =>
        parserState.basePrefix.map(withPrefix(resource)).get

      case PrefixedResource(prefix, name) => parserState.prefixes.find(_.prefix == prefix).map(withPrefix(name)).getOrElse {
        throw new TurtleParseException(s"Resource does not have a prefix with key $prefix in scope")
      }

      case unmatched => throw new TurtleParseException(s"turtle representation not in a form understood by the parser: $unmatched")
    }
  }


  val LabeledBlankNode = """^_:([^\s]*)$""".r
  private def resourceFromTurtle(turtleRepresentation: String, parserState: ParserState): RdfResource = {

    turtleRepresentation match {
      case LabeledBlankNode(label) => BlankNode(label)
      case resourceString => Resource(iriFromTurtle(resourceString, parserState))
    }
  }


  val StringLiteralWithLanguageMatcher = """^"(.*)"@(.*)$""".r
  val SimpleStringLiteralMatcher = """^"(.*)"$""".r
  val StringLiteralWithCustomIRIMatcher = """^"(.*)"\^\^(.*)$""".r
  val BooleanLiteralMatcher = """^(true|false)$""".r
  val IntegerLiteralMatcher = """^\+?(-?[0-9]+)$""".r
  val DecimalLiteralMatcher = """^\+?(-?[0-9]*\.[0-9]+)$""".r

  private def nodeFromTurtle(turtleRepresentation: String, parserState: ParserState): GraphNode = {

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

object RdfStandard {
  val prefixes = Seq(RdfOntology.prefix)
  val abrieviations: Map[String, String] = Map("a" -> RdfOntology.`type`.uri, "()" -> RdfOntology.nil.uri)
}

case class ParserState(basePrefix: Option[BasePrefix] = None,
                       prefixes: Seq[Prefix] = Nil,
                       partialTriple: PartialTriple = EmptyTriple) {

  def withPrefix(prefix: RdfPrefix): ParserState = prefix match {
    case basePrefix: BasePrefix =>
      copy(basePrefix = Some(basePrefix))
    case standardPrefix: Prefix =>
      val updatePrefixes = prefixes.filter(_.prefix != standardPrefix.prefix) :+ standardPrefix
      copy(prefixes = updatePrefixes)
  }

  def withPartial(partial: PartialTriple) = copy(partialTriple = partial)

}

object ParserState {
  val Empty = ParserState(None, RdfStandard.prefixes, EmptyTriple)
}

sealed abstract class PartialTriple
object EmptyTriple extends PartialTriple
case class Subject(subject: RdfResource) extends PartialTriple
case class SubjectAndPredicate(subject: RdfResource, predicate: Resource) extends PartialTriple
