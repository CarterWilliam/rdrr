package utilities

import rdrr.util.RandomStrings

trait PredictableRandomStrings extends RandomStrings {

  private var randomAlphaCounter = 0

  override def randomAlpha(n: Int) = {
    randomAlphaCounter += 1
    randomAlphaCounter.toString
  }

}

trait FaultyRandomStrings extends RandomStrings {

  private var randomAlphaCounter = 0

  override def randomAlpha(n: Int) = {
    randomAlphaCounter += 1
    (randomAlphaCounter/4 + 1).toString
  }

}
