package graphics

import chess.graphics.CFileChooser

class LoadFileChooser extends CFileChooser {
  title = "Load file."

  override def show(over: MainFrame): Result.Value = showOpenDialog(over)
}
