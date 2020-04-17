package graphics

import java.awt.event.KeyEvent

import framework.Input.{DrawOffer, Resign, TakebackProposal}

import scala.swing.{FileChooser, Menu, MenuBar}
import scala.swing.event

trait CMenuBar {
  self: CWindow =>

  menuBar = Menu

  private val reference: CWindow = this

  object Menu extends MenuBar {
    contents += new Menu("File") {
      mnemonic = event.Key.F
      contents += save
      contents += saveAs
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

    object takeback extends CMenuItem("Takeback", KeyEvent.VK_T, Shortcut.takeback, _ =>
      gameFrame.receiveInput(TakebackProposal))

    object resign extends CMenuItem("Resign", KeyEvent.VK_R, _ =>
      gameFrame.receiveInput(Resign))

    object restart extends CMenuItem("Restart", KeyEvent.VK_S, _ => {
      gameFrame.chessBoard = framework.ChessBoard.classicalBoard(gameFrame)
      gameFrame.repaint()
      gameFrame.unselectAll()
    })

    object draw extends CMenuItem("Draw", KeyEvent.VK_D, _ => {
      gameFrame.receiveInput(DrawOffer)
    })

    object save extends CMenuItem("Save", KeyEvent.VK_S, Shortcut.save, _ => gameFrame.save())

    object saveAs extends CMenuItem("Save as", KeyEvent.VK_A, Shortcut.saveAs, _ => gameFrame.saveAs())

    object load extends CMenuItem("Load", KeyEvent.VK_L, Shortcut.load, _ => {
      val fileChooser = new LoadFileChooser()
      val result = fileChooser.show(reference)
      result match {
        case f: FileChooser.Result.Value if f == FileChooser.Result.Approve =>
          val path = fileChooser.filePath
          val loaded = gameFrame.load(path)
          loaded match {
            case Some(errorMessage) =>
              Error error errorMessage.description
            case _ =>
          }
        case _ =>
      }
    })

  }

}
