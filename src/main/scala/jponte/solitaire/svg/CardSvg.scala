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
      _class := "w-24 h-32",
      xmlns  := "http://www.w3.org/2000/svg"
    )(
      rect(
        x      := "2%",
        y      := "2%",
        width  := "96%",
        height := "96%",
        Attribute("stroke-width", "1"),
        rx := "4%",
        ry := "4%",
        Attribute("stroke-linejoin", "round"),
        fill   := "#ffffff",
        stroke := "#333"
      ),
      textTag(
        x := "1em",
        y := "1.2em",
        Attribute("font-size", "16"),
        Attribute("text-anchor", "middle"),
        Attribute("alignment-baseline", "central"),
        fill := colorHex
      )(card.number.show),
      textTag(
        x := "1.9em",
        y := "1.2em",
        Attribute("font-size", "16"),
        Attribute("text-anchor", "middle"),
        Attribute("alignment-baseline", "central"),
        fill := colorHex
      )(card.suit.show),
      textTag(
        x := "50%",
        y := "50%",
        Attribute("font-size", "48"),
        Attribute("text-anchor", "middle"),
        Attribute("alignment-baseline", "central"),
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
      _class := "w-24 h-32",
      xmlns  := "http://www.w3.org/2000/svg"
    )(
      rect(
        x      := "2%",
        y      := "2%",
        width  := "96%",
        height := "96%",
        Attribute("stroke-width", "1"),
        rx := "4%",
        ry := "4%",
        Attribute("stroke-linejoin", "round"),
        fill   := "#eee",
        stroke := "#333"
      )
    )

  def cardPlaceholderSvg: Html[Msg] =
    svg(
      Attribute("version", "1.1"),
      _class := "w-24 h-32",
      xmlns  := "http://www.w3.org/2000/svg"
    )(
      rect(
        x      := "2%",
        y      := "2%",
        width  := "96%",
        height := "96%",
        Attribute("stroke-width", "1"),
        rx := "4%",
        ry := "4%",
        Attribute("stroke-linejoin", "round"),
        fill   := "transparent",
        stroke := "#333",
        Attribute("stroke-dasharray", "5,5")
      )
    )
}
