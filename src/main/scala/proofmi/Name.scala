package proofmi

/**
 * A name is a string with an integer index.
 */
case class Name(string: String, index: Int = 0)(implicit store: NameStore) {
  store.register(this)

  override def toString =
    if (index == 0) string
    else s"$string$index"

  def fresh(): Name = store.freshName(string)
}

object Name {
  implicit val store = new NameStore()

  /**
   * Decodes a string to a name.
   */
  val nameRegex = """([a-z]+)(\d*)""".r
  
  def decodeWithIndex(string: String, indexCode: String): Name = {
      val index = if (indexCode.isEmpty()) 0 else indexCode.toInt
      Name(string, index)
  }
  
  implicit def decode(code: String): Name = code match {
    case nameRegex(string, indexCode) => decodeWithIndex(string, indexCode)
  }
}