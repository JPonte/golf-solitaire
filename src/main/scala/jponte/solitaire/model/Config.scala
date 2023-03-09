package jponte.solitaire.model

case class Config(wrappingAllowed: Boolean, topKingAllowed: Boolean, noStartFoundation: Boolean)

object Config {
  val default: Config = Config(true, true, false)
}