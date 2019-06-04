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

  def unary_+ : Square = NumericSquare(colIndx, row)

  def unary_- : Square = NumericSquare(-colIndx, -row)

  def toTuple: (ColumnType, Int) = (column, row)

  def isValid: Boolean

  override def toString: String = s"$column$row"
}


final case class Square(column: Char, row: Int) extends AbstractSqrCoordinate[Char] {
  val colIndx: Int = columnIndex(column)

  /**
    * Checks if this square is inside the board.
    */
  override def isValid: Boolean = ChessBoard.isValidColumn(column) && row >= 1 && row <= 8

  override def +(sq: AbstractSqrCoordinate[_]): Square = sq match {
    case Square(col, line) =>
      Square(columnLetter(columnIndex(column)+columnIndex(col)), row + line)
    case NumericSquare(col, line) => Square(columnLetter(columnIndex(column) + col), row + line)
  }

  override def -(sq: AbstractSqrCoordinate[_]): Square = this.+(-sq)

  override def +(sq: (Int, Int)): Square = Square(columnLetter(columnIndex(column) + sq._1), row + sq._2)

  override def -(sq: (Int, Int)): Square = this.+(-sq._1, -sq._2)
}

object Square {
  def apply(coordinate: String): Option[Square] =
    if (coordinate.length == 2 && coordinate.head.isLetter && coordinate.last.isDigit) {
      val col = coordinate.head
      val row = coordinate.last.asDigit
      Some(Square(col, row))
    }
    else None
}


final case class NumericSquare(column: Int, row: Int) extends AbstractSqrCoordinate[Int] {
  def colIndx: Int = column

  /**
    * Checks if this square is inside the board.
    */
  override def isValid: Boolean = column >= 1 && column <= 8 && row >= 1 && row <= 8

  override def +(sq: AbstractSqrCoordinate[_]): NumericSquare = sq match {
    case Square(col, line) => NumericSquare(column + columnIndex(col), row + line)
    case NumericSquare(col, line) => NumericSquare(column + col, row + line)
  }

  override def -(sq: AbstractSqrCoordinate[_]): NumericSquare = this.+(-sq)

  override def +(sq: (Int, Int)): NumericSquare = NumericSquare(column + sq._1, row + sq._2)

  override def -(sq: (Int, Int)): NumericSquare = this.+(-sq._1, -sq._2)
}

object NumericSquare {
  def apply(tup: (Int, Int)) = new NumericSquare(tup._1, tup._2)
}


object AbstractSqrCoordinate {
  /**
    * implicit conversion from [[Square]] to [[NumericSquare]]
    */
  implicit def sqr2indxSqr(sqr: Square): NumericSquare = NumericSquare(sqr.colIndx, sqr.row)

  /**
    * implicit conversion from [[NumericSquare]] to [[Square]]
    */
  implicit def indxSqr2sqr(iSqr: NumericSquare): Square = Square(columnLetter(iSqr.column), iSqr.row)
}
