package utilities

import scala.io.Source

trait TestHelpers {
  def getResource(filename: String): String =
    Source.fromURL(getClass.getClassLoader.getResource(filename)).mkString
}
