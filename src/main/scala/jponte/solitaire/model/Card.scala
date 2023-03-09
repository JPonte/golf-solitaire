package jponte.solitaire.model

import cats.Show
import cats.syntax.all._

case class Card(number: Number, suit: Suit)

object Card:
  implicit val showCard: Show[Card] = (c: Card) => show"${c.number}${c.suit}"
