package chess.graphics

import scala.swing.FileChooser.Result
import swing._

trait CFileChooser extends FileChooser {
  multiSelectionEnabled = false
//  fileFilter = FileChooser.SelectionMode.FilesOnly

  def filePath: String = selectedFile.getAbsolutePath

  def show(over: MainFrame): Result.Value
}
