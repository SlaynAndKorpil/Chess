package chess.graphics

import java.awt.event.KeyEvent

import chess.framework._

import scala.swing._

trait CMenuBar {
  self: CWindow =>

  menuBar = Menu

  //TODO find better solution for self-reference when opening a FileChooser
  private val reference: CWindow = this

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

    object restart extends CMenuItem("Restart", KeyEvent.VK_S, _ => {
      gameFrame.board = chess.framework.ChessBoard.classicalBoard(gameFrame)
      gameFrame.repaint()
      gameFrame.unselectAll()
    })

    object save extends CMenuItem("Save", KeyEvent.VK_S, Shortcut.save, _ => {
      val fileChooser = new SaveFileChooser()
      val result = fileChooser.show(reference)
      result match {
        case f: FileChooser.Result.Value if f == FileChooser.Result.Approve =>
          import gameFrame._
          val path = fileChooser.filePath
          ChessBoard.save(board, path)
        case _ =>
      }
    })

    object load extends CMenuItem("Load", KeyEvent.VK_L, Shortcut.load, _ => {
      val fileChooser = new LoadFileChooser()
      val result = fileChooser.show(reference)
      result match {
        case f: FileChooser.Result.Value if f == FileChooser.Result.Approve =>
          import gameFrame._
          val path = fileChooser.filePath
          val loaded = ChessBoard.load(path)
          loaded match {
            case Some(x) =>
              board = x(gameFrame)
              gameFrame.repaint()
            case None =>
          }
        case _ =>
      }
    })

  }
}
