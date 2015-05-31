package rdrr.util

import scala.util.Random

trait RandomStrings {

  val EnglishAlphabet = 'a' to 'z'
  def randomString(alphabet: Seq[Char])(length: Int): String =
    Stream.continually(Random.nextInt(alphabet.size)).map(alphabet).take(length).mkString

  def randomAlpha(length: Int) = randomString(EnglishAlphabet)(length)
}
