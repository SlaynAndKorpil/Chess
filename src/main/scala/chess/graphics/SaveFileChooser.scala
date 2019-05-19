package chess.graphics

import scala.swing.FileChooser.Result
import swing._

class SaveFileChooser extends CFileChooser {
  title = "Save file."

  override def show(over: MainFrame): Result.Value = showSaveDialog(over)
}
