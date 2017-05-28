package rdrr.immutable.marshallers

import rdrr.immutable._

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

  private[this] def asUriTurtle(uri: String) =
    s"""<$uri>"""
  private[this] def asStandardStringLiteralTurtle(content: String) =
    s""""$content""""
  private[this] def asNonStandardStringLiteralTurtle(content: String, datatype: Resource) =
    s"""${asStandardStringLiteralTurtle(content)}^^${asUriTurtle(datatype.uri)}"""
  private[this] def asLanguageStringLiteralTurtle(content: String, language: String) =
    s""""$content"@$language"""
  private[this] def asLiteralTurtle(literal: Literal) =
    s"""${literal.value}^^${asUriTurtle(literal.datatype.uri)}"""


  private[this] def asTurtle(entity: RdfEntity): String = entity match {
    case Resource(uri) => RdfStandard.getOrElse(uri, asUriTurtle(uri))
    case BlankNode(label) => s"""_:$label"""
    case StandardStringLiteral(content) => asStandardStringLiteralTurtle(content)
    case NonStandardStringLiteral(string, datatype) => asNonStandardStringLiteralTurtle(string, datatype)
    case LanguageStringLiteral(content, language) => asLanguageStringLiteralTurtle(content, language)
    case otherLiteral: Literal => asLiteralTurtle(otherLiteral)
  }

}