package chess.graphics

import java.awt.event.KeyEvent

import chess.framework._

import scala.swing._

trait CMenuBar {
  self: CWindow =>

  menuBar = Menu

  /*
  TODO menus:
  TODO - propose takeback
  TODO - propose draw
  TODO - change board color scheme
  TODO - change main frame (main menu, chess puzzles, ...)
  TODO - ...
  */

  object Menu extends MenuBar {
    contents += new Menu("File") {
      mnemonic = event.Key.F
      contents += save
      contents += load
    }

    contents += new Menu("Game") {
      mnemonic = event.Key.G
      contents += resign
      contents += restart
      contents += Test
    }

    contents += new Menu("Graphics") {
      mnemonic = event.Key.R
      contents += boardColor
      contents += colorMode
    }

    object boardColor extends CMenuItem("Board color", KeyEvent.VK_B, _ => ())

    object colorMode extends CMenuItem("Color mode", KeyEvent.VK_C, _ => ())

    object resign extends CMenuItem("Resign", KeyEvent.VK_R, _ =>
      gameFrame.update(gameFrame.board.receive(Resign)))

    object restart extends CMenuItem("Restart", KeyEvent.VK_S, _ =>
      gameFrame.board = chess.framework.ChessBoard.classicalBoard(gameFrame))

    object save extends CMenuItem("Save", KeyEvent.VK_S, Shortcut.save, _ => {
      saveBoard("testSave")
      println("save!")
    })

    object load extends CMenuItem("Load", KeyEvent.VK_L, Shortcut.load, _ => println("load!"))

    @deprecated
    object Test extends CMenuItem("test promotion", 0, _ =>
      gameFrame.update(gameFrame.board.receive(Promotion(chess.framework.Queen)))
    )

  }

}
