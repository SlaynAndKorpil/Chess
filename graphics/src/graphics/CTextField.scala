package graphics

import scala.swing._

class CTextField(text: String) extends TextArea(text) {
  def this () = this("")

  background = new java.awt.Color(0x515355)
  foreground = new java.awt.Color(0xCCCCCC)

  xLayoutAlignment = peer.getWidth*100

  editable = false
}
