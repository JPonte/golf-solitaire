package jponte.solitaire

import scalajs.js

trait Msg

object Msg:
  case class ChoosePile(index: Int)   extends Msg
  case object ChooseDeck              extends Msg
  case class ClockTick(time: js.Date) extends Msg
