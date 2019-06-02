package chess.framework

import chess.framework.ChessBoard.{columnIndex, columnLetter}

import scala.language.implicitConversions

sealed abstract class AbstractSqrCoordinate[ColumnType] {
  val column: ColumnType
  val row: Int

  def _1: ColumnType = column

  def _2: Int = row

  def colIndx: Int

  def to(square: AbstractSqrCoordinate[_]): List[AbstractSqrCoordinate[_]] = {
    val inc = (square.colIndx - colIndx).signum -> (square.row - row).signum
    val incremented = this + inc
    this :: {
      if (incremented == this) Nil
      else incremented.to(square)
    }
  }

  def until[T](square: AbstractSqrCoordinate[_]): List[AbstractSqrCoordinate[_]] = {
    val inc = (square.colIndx - colIndx).signum -> (square.row - row).signum
    val incremented = this + inc
    this :: {
      if (incremented == square) Nil
      else incremented until square
    }
  }

  def +(sq: AbstractSqrCoordinate[_]): AbstractSqrCoordinate[_]

  def -(sq: AbstractSqrCoordinate[_]): AbstractSqrCoordinate[_]

  def +(sq: (Int, Int)): AbstractSqrCoordinate[_]

  def -(sq: (Int, Int)): AbstractSqrCoordinate[_]

  def unary_+ : SquareCoordinate = NumericSquareCoordinate(colIndx, row)

  def unary_- : SquareCoordinate = NumericSquareCoordinate(-colIndx, -row)

  def toTuple: (ColumnType, Int) = (column, row)

  def isValid: Boolean

  override def toString: String = s"$column$row"
}


final case class SquareCoordinate(column: Char, row: Int) extends AbstractSqrCoordinate[Char] {
  val colIndx: Int = columnIndex(column)

  /**
    * Checks if this square is inside the board.
    */
  override def isValid: Boolean = ChessBoard.isValidColumn(column) && row >= 1 && row <= 8

  override def +(sq: AbstractSqrCoordinate[_]): SquareCoordinate = sq match {
    case SquareCoordinate(col, line) =>
      SquareCoordinate(columnLetter(columnIndex(column)+columnIndex(col)), row + line)
    case NumericSquareCoordinate(col, line) => SquareCoordinate(columnLetter(columnIndex(column) + col), row + line)
  }

  override def -(sq: AbstractSqrCoordinate[_]): SquareCoordinate = this.+(-sq)

  override def +(sq: (Int, Int)): SquareCoordinate = SquareCoordinate(columnLetter(columnIndex(column) + sq._1), row + sq._2)

  override def -(sq: (Int, Int)): SquareCoordinate = this.+(-sq._1, -sq._2)
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

  override def +(sq: AbstractSqrCoordinate[_]): NumericSquareCoordinate = sq match {
    case SquareCoordinate(col, line) => NumericSquareCoordinate(column + columnIndex(col), row + line)
    case NumericSquareCoordinate(col, line) => NumericSquareCoordinate(column + col, row + line)
  }

  override def -(sq: AbstractSqrCoordinate[_]): NumericSquareCoordinate = this.+(-sq)

  override def +(sq: (Int, Int)): NumericSquareCoordinate = NumericSquareCoordinate(column + sq._1, row + sq._2)

  override def -(sq: (Int, Int)): NumericSquareCoordinate = this.+(-sq._1, -sq._2)
}

object NumericSquareCoordinate {
  def apply(tup: (Int, Int)) = new NumericSquareCoordinate(tup._1, tup._2)
}


object AbstractSqrCoordinate {
  /**
    * implicit conversion from [[SquareCoordinate]] to [[NumericSquareCoordinate]]
    */
  implicit def sqr2indxSqr(sqr: SquareCoordinate): NumericSquareCoordinate = NumericSquareCoordinate(sqr.colIndx, sqr.row)

  /**
    * implicit conversion from [[NumericSquareCoordinate]] to [[SquareCoordinate]]
    */
  implicit def indxSqr2sqr(iSqr: NumericSquareCoordinate): SquareCoordinate = SquareCoordinate(columnLetter(iSqr.column), iSqr.row)
}
