package chess.graphics

import chess.framework._
import javax.swing.Box

import scala.swing._

class PromotionChooser(val width: Int, val height: Int) extends Frame with Publisher with Reactor {
  peer setJMenuBar null
  peer setAlwaysOnTop true

  resizable = false

  peer.setSize(width, height)
  peer.setPreferredSize(new Dimension(width, height))
  peer.setMinimumSize(new Dimension(width, height))
  peer.setMaximumSize(new Dimension(width, height))

  location = new swing.Point(100, 100)

  reactions += {
    case promo: PromotionEvent =>
      publish(promo)
  }

  //TODO somehow it listens to itself
  deafTo(this)

  val backgroundCol: BoardColors.BoardColor = BoardColors.Brown.White

  private val content: Seq[Component] = Seq(
    new PromoOptionButton(White, Queen, backgroundCol),
    new PromoOptionButton(White, Rook, backgroundCol),
    new PromoOptionButton(White, Knight, backgroundCol),
    new PromoOptionButton(White, Bishop, backgroundCol)
  )

  content foreach {
    case promo: PromoOptionButton => listenTo(promo)
    case _ =>
  }

  contents = new BoxPanel(Orientation.Horizontal) {
    contents ++= content
    peer add Box.createVerticalStrut(2)
  }
}
