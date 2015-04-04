package rdrr.util

trait JavaHelpers {
  def closeWhenDone[T <: { def close(): Unit }, R](closable: T)(operation: T => R) = {
    val result = operation(closable)
    closable.close()
    result
  }
}
