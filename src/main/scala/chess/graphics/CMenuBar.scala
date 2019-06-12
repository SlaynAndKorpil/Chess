package chess.graphics

import java.awt.event.KeyEvent

import chess.framework.Input.{DrawOffer, Resign, TakebackProposal}
import chess.framework._

import scala.swing._

trait CMenuBar {
  self: CWindow =>

  menuBar = Menu

  //TODO find better solution for self-reference when opening a FileChooser
  private val reference: CWindow = this

  object Menu extends MenuBar {
    contents += new Menu("File") {
      mnemonic = event.Key.F
      contents += save
      contents += load
    }

    contents += new Menu("Game") {
      mnemonic = event.Key.G
      contents += takeback
      contents += resign
      contents += draw
      contents += restart
    }

    contents += new Menu("Graphics") {
      mnemonic = event.Key.R
      contents += boardColor
      contents += colorMode
    }

    object boardColor extends CMenuItem("Board color", KeyEvent.VK_B, _ => ())

    object colorMode extends CMenuItem("Color mode", KeyEvent.VK_C, _ => ())

    object takeback extends CMenuItem("Takeback", KeyEvent.VK_T, _ =>
      gameFrame.receiveInput(TakebackProposal))

    object resign extends CMenuItem("Resign", KeyEvent.VK_R, _ =>
      gameFrame.receiveInput(Resign))

    object restart extends CMenuItem("Restart", KeyEvent.VK_S, _ => {
      gameFrame.chessBoard = chess.framework.ChessBoard.classicalBoard(gameFrame)
      gameFrame.repaint()
      gameFrame.unselectAll()
    })

    object draw extends CMenuItem("Draw", KeyEvent.VK_D, _ => {
      gameFrame.receiveInput(DrawOffer)
    })

    object save extends CMenuItem("Save", KeyEvent.VK_S, Shortcut.save, _ => {
      val fileChooser = new SaveFileChooser()
      val result = fileChooser.show(reference)
      result match {
        case f: FileChooser.Result.Value if f == FileChooser.Result.Approve =>
          import gameFrame._
          val path = fileChooser.filePath
          ChessBoard.save(chessBoard, path)
        case _ =>
      }
    })

    object load extends CMenuItem("Load", KeyEvent.VK_L, Shortcut.load, _ => {
      val fileChooser = new LoadFileChooser()
      val result = fileChooser.show(reference)
      result match {
        case f: FileChooser.Result.Value if f == FileChooser.Result.Approve =>
          import gameFrame.{io, chessBoard}
          val path = fileChooser.filePath
          val loaded = ChessBoard.loadExactPath(path)
          loaded match {
            case Right(board) =>
              chessBoard = board
              gameFrame.repaint()
              gameFrame.displayCheck()
            case Left(errorMessage) =>
              Error error errorMessage.description
          }
        case _ =>
      }
    })

  }

}
