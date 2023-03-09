package jponte

import cats.effect.IO
import jponte.solitaire.Msg
import tyrian.Html.*
import tyrian.*

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import scala.concurrent.duration.*
import scala.scalajs.js.annotation.*

import scalajs.js
import solitaire.model.Card
import solitaire.model.Config
import solitaire.model.Game
import solitaire.model.Number
import solitaire.model.Suit
import solitaire.svg.CardSvg

@JSExportTopLevel("TyrianApp")
object Solitaire extends TyrianApp[Msg, Model]:

  def init(flags: Map[String, String]): (Model, Cmd[IO, Msg]) =
    val deck: List[Card] =
      (for
        number <- Number.values
        suit   <- Suit.values
      yield Card(number, suit)).toList

    val game = Game.drawGame(
      7,
      5,
      Game(
        deck,
        List(),
        List(),
        1,
        0,
        (new js.Date()).getTime(),
        (new js.Date()).getTime()
      )
    )(Config.default)

    (game, Cmd.None)

  def update(model: Model): Msg => (Model, Cmd[IO, Msg]) =
    case Msg.ChoosePile(index) =>
      val newGame = Game.choosePile(index, model)._1
      val resolvedGame = if (Game.hasPlaysLeft(newGame)) {
        newGame
      } else {
        Game.resolveGame(model)
      }
      (resolvedGame, Cmd.None)
    case Msg.ChooseDeck =>
      val newGame = Game.chooseDeck(model)._1
      val resolvedGame = if (Game.hasPlaysLeft(newGame)) {
        newGame
      } else {
        Game.resolveGame(model)
      }
      (resolvedGame, Cmd.None)
    case Msg.ClockTick(time) =>
      (model.copy(currentTime = time.getTime()), Cmd.None)

  def view(model: Model): Html[Msg] =
    val duration = Duration(
      model.currentTime - model.startTime,
      java.util.concurrent.TimeUnit.MILLISECONDS
    )
    div(
      _class := "h-screen w-full flex justify-center items-center"
    )(
      div(_class := "h-fit w-fit p-2 flex flex-col gap-2")(
        div()(
          div(_class := "flex justify-end font-sans font-medium")(
            f"${duration.toSeconds / 60}%02d:${duration.toSeconds % 60}%02d"
          ),
          div(_class := "flex justify-end font-sans font-medium")(
            f"Hole: ${model.hole}"
          ),
          div(_class := "flex justify-end font-sans font-medium")(
            f"Score: ${model.score}"
          )
        ),
        div(_class := "w-fit h-72 flex flex-row gap-3")(
          model.tableau.zipWithIndex.map { case (pile, pileIndex) =>
            div(style := "display: flex; flex-direction: column;")(
              if (pile.isEmpty) {
                List(CardSvg.cardPlaceholderSvg)
              } else {
                pile.reverse.zipWithIndex.map { case (card, cardIndex) =>
                  div(_class := "w-13 h-6")(
                    CardSvg.cardButton(
                      card,
                      Option.when(cardIndex == pile.size - 1)(
                        Msg.ChoosePile(pileIndex)
                      )
                    )
                  )
                }
              }
            )
          }
        ),
        div(_class := "h-fit w-full flex flex-row")(
          div(_class := "flex-grow flex flex-row")(
            model.foundation.reverse.map { card =>
              div(_class := "w-1 h-1")(
                CardSvg.cardButton(card, None)
              )
            }
          ),
          div(
            _class := "flex-none w-fit flex flex-row items-start justify-end"
          )(
            model.deck.headOption match
              case None        => CardSvg.cardPlaceholderSvg
              case Some(value) => CardSvg.blankCardButton(Some(Msg.ChooseDeck))
          )
        )
      )
    )

  def subscriptions(model: Model): Sub[IO, Msg] =
    Sub.every[IO](1.second, "clock-ticks").map(Msg.ClockTick(_))

type Model = Game
