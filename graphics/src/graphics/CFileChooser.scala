package graphics

import scala.swing.FileChooser.Result
import scala.swing.{FileChooser, MainFrame}

/**
  * [Description]
  *
  * @author Felix Lehner
  * @version
  */
trait CFileChooser extends FileChooser {
  multiSelectionEnabled = false
//  fileFilter = FileChooser.SelectionMode.FilesOnly

  def filePath: String = selectedFile.getAbsolutePath

  def show(over: MainFrame): Result.Value
}
