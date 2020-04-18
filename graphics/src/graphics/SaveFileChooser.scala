package graphics

import scala.swing.FileChooser.Result
import scala.swing.MainFrame

class SaveFileChooser extends CFileChooser {
  title = "Save file."

  override def show(over: MainFrame): Result.Value = showSaveDialog(over)
}
