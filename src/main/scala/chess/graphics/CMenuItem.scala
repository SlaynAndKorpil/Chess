package chess.graphics

import javax.swing.KeyStroke
import scala.swing._

/**
  * A wrapper for [[MenuItem]] witch represents the special needs of a chess menu item.
  */
class CMenuItem private (name: String, val mnemonicKey: Int, key: Option[KeyStroke], val function: Unit => Unit) extends MenuItem (
  new Action (name) {
    accelerator = key
    mnemonic = mnemonicKey
    def apply (): Unit = function()
  }) {
  /**
    * @see java.awt.event.KeyEvent (mnemonics)
    * @param name the displayed name
    * @param mnemonic button press to use this item
    * @param function the function to be used when this menu item is pressed
    */
  def this (name: String, mnemonic: Int, function: Unit => Unit) = this(name, mnemonic, None, function)

  /**
    * @param name the displayed name
    * @param mnemonic button sth.
    * @param key shortcut
    * @param function the function to be used
    */
  def this (name: String, mnemonic: Int, key: String, function: Unit => Unit) = this(name, mnemonic, Some(KeyStroke.getKeyStroke(key)), function)
}
