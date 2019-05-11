package chess.graphics

import scala.swing._

class CWindow (width: Int = 1080, height: Int = 1080) extends MainFrame with CMenuBar {
  title = "Chess"

  preferredSize = new Dimension(width, height)

  val gameFrame = new Board

  contents = gameFrame

  def saveBoard (file: String): Unit = gameFrame.saveBoard(file)

  def display () {
    pack
    visible = true
  }
}
