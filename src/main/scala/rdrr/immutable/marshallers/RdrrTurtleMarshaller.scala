package rdrr.immutable.marshallers

import rdrr.immutable._

import scala.io.Source

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