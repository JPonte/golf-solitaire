package jponte.solitaire.model

import cats.Show

enum Suit(val color: SuitColor):
  case Hearts   extends Suit(SuitColor.Red)
  case Diamonds extends Suit(SuitColor.Red)
  case Clubs    extends Suit(SuitColor.Black)
  case Spades   extends Suit(SuitColor.Black)

object Suit:
  implicit val showSuit: Show[Suit] = (s: Suit) =>
    s match
      case Suit.Hearts   => "♥"
      case Suit.Diamonds => "♦"
      case Suit.Clubs    => "♣"
      case Suit.Spades   => "♠"
