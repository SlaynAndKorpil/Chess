package graphics

import scala.swing._

class CWindow(width: Int = 1080, height: Int = 1080, loadGameFrom: String) extends MainFrame with CMenuBar {
  title = "Chess"

  preferredSize = new Dimension(width, height)

  val gameFrame = new Board(this, loadGameFrom)

  contents = gameFrame

  def saveBoard(file: String): Unit = gameFrame.saveBoard(file)

  def display() {
    pack
    visible = true
  }
}
