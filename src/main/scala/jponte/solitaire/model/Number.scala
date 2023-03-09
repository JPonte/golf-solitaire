package jponte.solitaire.model

import cats.Show

enum Number:
  case Ace, Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Valet, Dame,
  King

object Number {
  implicit val showNumber: Show[Number] = (n: Number) =>
    n match
      case Number.Ace   => "A"
      case Number.Two   => "2"
      case Number.Three => "3"
      case Number.Four  => "4"
      case Number.Five  => "5"
      case Number.Six   => "6"
      case Number.Seven => "7"
      case Number.Eight => "8"
      case Number.Nine  => "9"
      case Number.Ten   => "10"
      case Number.Valet => "J"
      case Number.Dame  => "Q"
      case Number.King  => "K"
}
