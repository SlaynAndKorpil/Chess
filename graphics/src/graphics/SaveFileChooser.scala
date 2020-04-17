package graphics

import chess.graphics.CFileChooser

class SaveFileChooser extends CFileChooser {
  title = "Save file."

  override def show(over: MainFrame): Result.Value = showSaveDialog(over)
}
