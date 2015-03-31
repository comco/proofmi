package proofmi

import scala.collection.mutable.{Map => MutableMap}

/**
 * NameStore stores names and provides fresh ones.
 */
class NameStore {
  implicit def store = this
  
  private val nextIndex = MutableMap.empty[String, Int].withDefaultValue(0)
 
  /**
   * Returns a fresh name based on a string.
   */
  def freshName(string: String) = {
    val index = nextIndex(string)
    nextIndex(string) += 1
    Name(string, index)
  }
  
  /**
   * Registers a name with this name store.
   */
  def register(name: Name) {
    val index = nextIndex(name.string)
    nextIndex(name.string) = Math.max(index, 1 + name.index)
  }
}

object NameStore {
  implicit val store = new NameStore
}