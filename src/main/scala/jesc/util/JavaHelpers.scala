package jesc.util

trait JavaHelpers {
  def using[T <: { def close(): Unit }, R](closable: T)(operation: T => R) = {
    val result = operation(closable)
    closable.close()
    result
  }
}
