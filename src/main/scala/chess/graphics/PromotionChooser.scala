package chess.graphics

import javax.swing.Box

import scala.swing._

class PromotionChooser extends Frame {
  peer setJMenuBar null
  peer setAlwaysOnTop true

  resizable = false

  contents = new BoxPanel(Orientation.Horizontal) {
    contents += new Square(BoardColors.Brown.White, chess.framework.SquareCoordinate('a', 1), chess.framework.Bishop(chess.framework.White))
    peer add Box.createVerticalStrut(2)
  }
}
