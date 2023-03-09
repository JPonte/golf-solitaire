package jponte.solitaire.svg

import cats.syntax.all._
import jponte.solitaire.Msg
import jponte.solitaire.model.Card
import jponte.solitaire.model.SuitColor
import tyrian.Html.*
import tyrian.SVG.*
import tyrian.*

object CardSvg {

  def cardButton(card: Card, onClickMsg: Option[Msg]): Html[Msg] =
    onClickMsg match
      case None =>
        cardSvg(card)
      case Some(msg) =>
        button(
          style := "background-color: transparent; border: transparent;",
          onClick(msg)
        )(
          cardSvg(card)
        )

  def cardSvg(card: Card): Html[Msg] =
    val colorHex = card.suit.color match
      case SuitColor.Red   => "#ff0000"
      case SuitColor.Black => "#000000"
    svg(
      Attribute("version", "1.1"),
      width  := "52",
      height := "72",
      xmlns  := "http://www.w3.org/2000/svg"
    )(
      rect(
        x      := "1",
        y      := "1",
        width  := "50",
        height := "70",
        Attribute("stroke-width", "1"),
        rx := "4",
        ry := "4",
        Attribute("stroke-linejoin", "round"),
        fill   := "#ffffff",
        stroke := "#333"
      ),
      textTag(
        x := "8",
        y := "16",
        Attribute("font-size", "16"),
        Attribute("text-anchor", "middle"),
        fill := colorHex
      )(card.number.show),
      textTag(
        x := "20",
        y := "16",
        Attribute("font-size", "16"),
        Attribute("text-anchor", "middle"),
        fill := colorHex
      )(card.suit.show),
      textTag(
        x := "26",
        y := "60",
        Attribute("font-size", "48"),
        Attribute("text-anchor", "middle"),
        fill := colorHex
      )(card.number.show)
    )

  def blankCardButton(onClickMsg: Option[Msg]): Html[Msg] =
    onClickMsg match
      case None =>
        blankCardSvg
      case Some(msg) =>
        button(
          style := "background-color: transparent; border: transparent;",
          onClick(msg)
        )(
          blankCardSvg
        )

  def blankCardSvg: Html[Msg] =
    svg(
      Attribute("version", "1.1"),
      width  := "52",
      height := "72",
      xmlns  := "http://www.w3.org/2000/svg"
    )(
      rect(
        x      := "1",
        y      := "1",
        width  := "50",
        height := "70",
        Attribute("stroke-width", "1"),
        rx := "4",
        ry := "4",
        Attribute("stroke-linejoin", "round"),
        fill   := "#eee",
        stroke := "#333"
      )
    )

  def cardPlaceholderSvg: Html[Msg] =
    svg(
      Attribute("version", "1.1"),
      width  := "52",
      height := "72",
      xmlns  := "http://www.w3.org/2000/svg"
    )(
      rect(
        x      := "1",
        y      := "1",
        width  := "50",
        height := "70",
        Attribute("stroke-width", "1"),
        rx := "4",
        ry := "4",
        Attribute("stroke-linejoin", "round"),
        fill   := "transparent",
        stroke := "#333",
        Attribute("stroke-dasharray", "5,5")
      )
    )
}
