package daisy

import lang.Identifiers.Identifier
import lang.Identifiers._

// Custom exception that holds a list of Identifiers
class OverflowFixException(
  val identifiers: List[Identifier],
  message: String = "",
  cause: Throwable = null
) extends Exception(message, cause) {

  // Override getMessage to include multiple Identifier information
  override def getMessage: String = {
    val baseMessage = if (message.nonEmpty) s"$message\n" else ""
    val names = identifiers.map(_.name).mkString(", ")
    s"${baseMessage}Identifier details: names=[$names]"
  }
}