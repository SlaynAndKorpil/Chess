package chess.framework

import scala.language.implicitConversions

abstract class AbstractSqrCoordinate[ColumnType] {
  val column: ColumnType
  val row: Int

  def _1: ColumnType = column

  def _2: Int = row

  def colIndx: Int

  def to(square: SquareCoordinate): List[SquareCoordinate] = {
    val inc = ((square.colIndx - colIndx).signum, (square.row - row).signum)
    val incremented = this + inc
    if (inc._1 == 0 && inc._2 == 0) Nil
    else incremented :: incremented.to(square)
  }

  def until(square: SquareCoordinate): List[SquareCoordinate] = {
    if (square.colIndx - colIndx > 1 || square.row - row > 1 || square.colIndx - colIndx < -1 || square.row - row < -1) {
      val inc = ((square.colIndx - colIndx).signum, (square.row - row).signum)
      if (inc._1 == 0 && inc._2 == 0) Nil
      else NumericSquareCoordinate(colIndx, row) :: (this + inc).to(square)
    }
    else Nil
  }

  def +(sq: AbstractSqrCoordinate[_]): SquareCoordinate = this + (sq.colIndx, sq.row)

  def -(sq: AbstractSqrCoordinate[_]): SquareCoordinate = this - (sq.colIndx, sq.row)

  def +(sq: (Int, Int)): SquareCoordinate = SquareCoordinate(ChessBoard.columnLetter(colIndx + sq._1), row + sq._2)

  def -(sq: (Int, Int)): SquareCoordinate = this + (-sq._1, -sq._2)

  def unary_+ : SquareCoordinate = NumericSquareCoordinate(colIndx, row)

  def unary_- : SquareCoordinate = NumericSquareCoordinate(-colIndx, -row)

  def toTuple: (ColumnType, Int) = (column, row)

  def isValid: Boolean

  override def toString: String = s"$column$row"
}


final case class SquareCoordinate(column: Char, row: Int) extends AbstractSqrCoordinate[Char] {
  def colIndx: Int = ChessBoard.columnIndex(column)

  /**
    * Checks if this square is inside the board.
    */
  override def isValid: Boolean = ChessBoard.isValidColumn(column) && row >= 1 && row <= 8
}

object SquareCoordinate {
  def apply(coordinate: String): Option[SquareCoordinate] =
    if (coordinate.length == 2 && coordinate.head.isLetter && coordinate.last.isDigit) {
      val col = coordinate.head
      val row = coordinate.last.asDigit
      Some(SquareCoordinate(col, row))
    }
    else None
}


final case class NumericSquareCoordinate(column: Int, row: Int) extends AbstractSqrCoordinate[Int] {
  def colIndx: Int = column

  /**
    * Checks if this square is inside the board.
    */
  override def isValid: Boolean = column >= 1 && column <= 8 && row >= 1 && row <= 8
}

object NumericSquareCoordinate {
  def apply(tup: (Int, Int)) = new NumericSquareCoordinate(tup._1, tup._2)
}


object AbstractSqrCoordinate {
  /**
    * implicit conversion from [[SquareCoordinate]] to [[NumericSquareCoordinate]]
    */
  implicit def sqr2indxSqr(sqr: SquareCoordinate): NumericSquareCoordinate = NumericSquareCoordinate(ChessBoard.columnIndex(sqr.column), sqr.row)

  /**
    * implicit conversion from [[NumericSquareCoordinate]] to [[SquareCoordinate]]
    */
  implicit def indxSqr2sqr(iSqr: NumericSquareCoordinate): SquareCoordinate = SquareCoordinate(ChessBoard.columnLetter(iSqr.column), iSqr.row)
}
