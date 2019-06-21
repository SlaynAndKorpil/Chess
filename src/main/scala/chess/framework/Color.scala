package chess.framework

sealed trait Color {
  def opposite: Color
}

object Color {
  val NOCOLOR = "noColor"
  val WHITE = "white"
  val BLACK = "black"

  def apply (c: String): Color = c.toLowerCase match {
    case WHITE => White
    case BLACK => Black
    case _ => NoColor
  }
  def apply (c: Char): Color = c.toLower match {
    case 'w' => White
    case 'b' => Black
    case _ => NoColor
  }

  def unapply (c: Color): String = c match {
    case White => WHITE
    case Black => BLACK
    case NoColor => NOCOLOR
  }
}

sealed trait AnyColor extends Color {
  override def opposite: AnyColor
}

case object White extends AnyColor {
  override def toString: String = Color.WHITE
  override def opposite: AnyColor = Black
}

case object Black extends AnyColor {
  override def toString: String = Color.BLACK
  override def opposite: AnyColor = White
}

case object NoColor extends Color {
  override def toString: String = Color.NOCOLOR
  override def opposite: Color = this
}
