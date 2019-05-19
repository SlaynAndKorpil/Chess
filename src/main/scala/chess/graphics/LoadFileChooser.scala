package chess.graphics

import scala.swing.FileChooser.Result
import swing._

class LoadFileChooser extends CFileChooser {
  title = "Load file."

  override def show(over: MainFrame): Result.Value = showOpenDialog(over)
}
