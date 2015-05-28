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
    val EntityEtc = """^\s*([^\s'"]*[^\s'";,.])\s*(.*)$""".r // Resources, Literals, Labeled Blank Nodes, unlabeled blank node open/close with content ...
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
                         private val scopedPartialTriples: Seq[PartialTriple] = Seq(EmptyTriple)) {

    lazy val partialTriple = scopedPartialTriples.headOption.getOrElse{
      throw new TurtleParseException("Error getting parser scope. Additional blank node close brace in turtle ']' ?")
    }

    def + (prefix: RdfPrefix): ParserState = prefix match {
      case basePrefix: BasePrefix =>
        copy(basePrefix = Some(basePrefix))
      case standardPrefix: Prefix =>
        val updatePrefixes = prefixes.filter(_.prefix != standardPrefix.prefix) :+ standardPrefix
        copy(prefixes = updatePrefixes)
    }

    def + (node: BlankNode): ParserState = copy(blankNodes = node +: blankNodes)

    def withPartial(partialTriple: PartialTriple) =
      copy(scopedPartialTriples = partialTriple +: scopedPartialTriples.tail)

    def nextBlankNode: BlankNode = {
      def iteration(n: Int): BlankNode = {
        if (blankNodes contains BlankNode("blank-" + n))
          iteration(n+1)
        else
          BlankNode("blank-" + n)
      }
      iteration(1)
    }

    def pushBlankNodeScope() = {
      val newBlankNode = nextBlankNode
      scopedPartialTriples.head match {
        case EmptyTriple =>
          copy(blankNodes = newBlankNode +: blankNodes, scopedPartialTriples = Subject(newBlankNode) +: Subject(newBlankNode) +: scopedPartialTriples.tail)
        case _ =>
          copy(blankNodes = newBlankNode +: blankNodes, scopedPartialTriples = Subject(newBlankNode) +: scopedPartialTriples)
      }

    }

    def popBlankNodeScope() = copy(scopedPartialTriples = scopedPartialTriples.tail)

  }

  object ParserState {
    val Empty = ParserState(None, Nil, Nil, Seq(EmptyTriple))
  }

  sealed abstract class PartialTriple
  object EmptyTriple extends PartialTriple {
    override def toString = "EmptyTriple"
  }
  case class Subject(subject: RdfResource) extends PartialTriple
  case class SubjectAndPredicate(subject: RdfResource, predicate: Predicate) extends PartialTriple

  private[this] def triplesFromEntities(entities: Stream[String],
                                        parserState: ParserState = ParserState.Empty): Stream[Triple] = {

    val BasePrefixExtractor = """^@(?:base|BASE)\s+<(.*)>\s*\.$""".r
    val PrefixExtractor = """^@(?:prefix|PREFIX)\s+(.*):\s*<(.*)>\s*\.$""".r
    val AnonymousBlankNode = "[]"
    val AnonymousBlankNodeStart = "["
    val AnonymousBlankNodeEnd = "]"
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
          entity match {

            case AnonymousBlankNode =>
              val blankNode = parserState.nextBlankNode
              triplesFromEntities(rest, parserState + blankNode withPartial Subject(blankNode))

            case AnonymousBlankNodeStart =>
              val blankNode = parserState.nextBlankNode
              triplesFromEntities(rest, parserState.pushBlankNodeScope())

            case resourceTurtle =>
              triplesFromEntities(rest, parserState.withPartial(Subject(resourceFromTurtle(entity, parserState))))

          }

        case Subject(subject) =>
          val subjectPredicatePartial = SubjectAndPredicate(subject, Predicate(iriFromTurtle(entity, parserState)))
          triplesFromEntities(rest, parserState.withPartial(subjectPredicatePartial))

        case SubjectAndPredicate(subject, predicate) => {
          entity match {

            case AnonymousBlankNodeStart =>
              val newState = parserState.pushBlankNodeScope()
              Triple(subject, predicate, newState.blankNodes.head) #:: triplesFromEntities(rest, newState)
            case AnonymousBlankNodeEnd =>
              triplesFromEntities(rest, parserState.popBlankNodeScope())

            case AnotherObjectNext =>
              triplesFromEntities(rest, parserState.withPartial(SubjectAndPredicate(subject, predicate)))
            case AnotherPredicateNext =>
              triplesFromEntities(rest, parserState.withPartial(Subject(subject)))
            case AnotherSubjectNext =>
              triplesFromEntities(rest, parserState.withPartial(EmptyTriple))

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
    val EmailAddress = """^<(\S+@\S+\.\S+)>$""".r // email
    val RelativeUriResource = "<(.*)>".r
    val PrefixedResource = "(.*):(.*)".r

    def withPrefix(resource: String)(prefix: RdfPrefix) = prefix.path + resource

    turtleRepresentation match {

      case rdfStandard if RdfStandard.contains(rdfStandard) => RdfStandard(rdfStandard)

      case UriResource(uri) => uri

      case EmailAddress(email) => email

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
