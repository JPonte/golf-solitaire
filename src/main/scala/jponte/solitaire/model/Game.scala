package jponte.solitaire.model

import cats.Applicative
import cats.Show
import cats.Traverse
import cats.data.State
import cats.data.StateT
import cats.effect.IO
import cats.effect.kernel.Resource.Eval
import cats.effect.unsafe.implicits.*
import cats.syntax.all.*
import cats.syntax.all.*
import jponte.solitaire.model.*

import scala.util.Random

case class Game(
    deck: List[Card],
    tableau: List[List[Card]],
    foundation: List[Card],
    hole: Int,
    score: Int,
    startTime: Double,
    currentTime: Double
)

object Game:
  implicit val showGame: Show[Game] = (g: Game) => {
    val maxPileSize = g.tableau.map(_.size).max
    val tableauStr = (0 until maxPileSize)
      .map { depth =>
        g.tableau
          .map(pile => pile.reverse.get(depth).map(_.show).getOrElse("   "))
          .mkString("\t")
      }
      .mkString("\n")
    val foundation = g.foundation.headOption.map(_.show).getOrElse("")
    s"$tableauStr\n\n${g.deck.size} | $foundation"
  }

  def shuffleDeck(game: Game): Game =
    game.copy(deck = Random.shuffle(game.deck))

  def drawPile(nCards: Int, pileIndex: Int, game: Game): Game =
    val newPile    = game.deck.take(nCards)
    val newDeck    = game.deck.takeRight(game.deck.size - nCards)
    val newTableau = game.tableau.patch(pileIndex, List(newPile), 1)
    game.copy(deck = newDeck, tableau = newTableau)

  def drawGame(piles: Int, pileSize: Int, game: Game)(config: Config): Game =
    val shuffled = shuffleDeck(game)
    val drawn =
      (0 until piles).foldLeft(shuffled)((g, i) => drawPile(pileSize, i, g))
    if (config.noStartFoundation)
      drawn
    else
      chooseDeck(drawn)._1

  def isPlayAllowed(tableauCard: Card, foundationCard: Card)(
      config: Config
  ): Boolean =
    val i1 = Number.values.indexOf(tableauCard.number)
    val i2 = Number.values.indexOf(foundationCard.number)
    if (!config.topKingAllowed && foundationCard.number == Number.King)
      false
    else if (Math.abs(i1 - i2) == 1)
      true
    else if (
      config.wrappingAllowed && ((i1 == 0 && i2 == Number.values.length - 1) || (i2 == 0 && i1 == Number.values.length - 1))
    )
      true
    else
      false

  def choosePile(index: Int, game: Game): (Game, Boolean) =
    if (index < 0 || index >= game.tableau.size) {
      (game, false)
    } else {
      val selectedPile = game.tableau(index)
      if (selectedPile.isEmpty) {
        (game, false)
      } else {
        val (pile, card)  = (selectedPile.tail, selectedPile.head)
        val foundationTop = game.foundation.head
        if (!isPlayAllowed(card, foundationTop)(Config.default)) {
          (game, false)
        } else {
          val newPiles      = game.tableau.patch(index, List(pile), 1)
          val newFoundation = game.foundation.prepended(card)
          val newGame =
            game.copy(tableau = newPiles, foundation = newFoundation)
          (newGame, true)
        }
      }
    }

  def chooseDeck(game: Game): (Game, Boolean) =
    if (game.deck.isEmpty) {
      (game, false)
    } else {
      val (newDeck, card) = (game.deck.tail, game.deck.head)
      val newFoundation   = game.foundation.prepended(card)
      val newGame = game.copy(deck = newDeck, foundation = newFoundation)
      (newGame, true)
    }

  def hasPlaysLeft(game: Game): Boolean =
    if (game.deck.nonEmpty) {
      true
    } else {
      game.foundation.headOption match {
        case None => game.tableau.flatten.nonEmpty
        case Some(foundationCard) =>
          game.tableau
            .flatMap(_.headOption)
            .exists(tableauCard =>
              isPlayAllowed(tableauCard, foundationCard)(Config.default)
            )
      }
    }

  def resolveGame(game: Game): Game =
    val score =
      if (game.tableau.flatten.size > 0)
        game.tableau.flatten.size
      else
        -game.deck.size
    val newDeck: List[Card] =
      (for
        number <- Number.values
        suit   <- Suit.values
      yield Card(number, suit)).toList
    val newGame =
      game.copy(
        foundation = List(),
        deck = newDeck,
        hole = game.hole + 1,
        score = game.score + score
      )
    Game.drawGame(7, 5, newGame)(Config.default)
