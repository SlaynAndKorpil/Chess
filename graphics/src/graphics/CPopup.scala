package graphics

import scala.swing._

object CDialog {
  def showConfirmation(message: String, title: String, onSuccess: () => Unit, onRejection: () => Unit)
                      (implicit parent: Component): Unit =
    Dialog.showConfirmation(parent, message, title) match {
      case Dialog.Result.Cancel => onRejection()
      case Dialog.Result.Closed => onRejection()
      case Dialog.Result.Yes => onSuccess()
      case Dialog.Result.No => onRejection()
    }

  def showMessage(message: String, title: String)(implicit parent: Component): Unit =
    Dialog.showMessage(parent, message, title)
}
