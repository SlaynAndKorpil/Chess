package graphics

import scala.swing.FileChooser.Result
import scala.swing.MainFrame

class LoadFileChooser extends CFileChooser {
  title = "Load file."

  override def show(over: MainFrame): Result.Value = showOpenDialog(over)
}
