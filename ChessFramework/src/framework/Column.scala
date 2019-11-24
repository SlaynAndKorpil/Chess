package framework

import scala.collection.generic.CanBuildFrom
import scala.collection.mutable.ArrayBuffer
import scala.collection.{IndexedSeqLike, mutable}
import scala.language.postfixOps
import scala.xml.Elem

/**
  * A column that contains 8 pieces.
  *
  * @see [[framework.ChessBoard]]
  * @version alpha 0.1
  * @author Felix Lehner
  */
final class Column(ps: Array[Piece]) extends IndexedSeq[Piece] with IndexedSeqLike[Piece, Column] {
  val length = 8

  val pieces: Array[Piece] = if (ps.length == 8) ps else Array.fill(8)(NoPiece)

  def this(ps: Map[Int, Piece]) =
    this(ps.toArray.foldRight(Array.fill[Piece](8)(NoPiece)) {
      (a: (Int, Piece), b: Array[Piece]) =>
        if (a._1 <= 8 && a._1 >= 1) b.updated(a._1 - 1, a._2)
        else b
    })

  /** @return a [[framework.Column]] that is filled with a certain piece */
  def this(piece: Piece) =
    this(Array.fill(8)(piece))

  /** @return the piece at a specific position in the array */
  def pieceAt(row: Int): Piece = pieces(row - 1)

  def apply(idx: Int): Piece = pieces(idx)

  def updated(line: Int, piece: Char, color: AnyColor): Column = updated(line, piece.toUpper match {
    case 'P' => Pawn(color)
    case 'R' => Rook(color)
    case 'N' => Knight(color)
    case 'B' => Bishop(color)
    case 'K' => King(color)
    case 'Q' => Queen(color)
    case _ => NoPiece
  })

  /**
    * Replaces a piece at a specific position in [[pieces]].
    *
    * @note the line parameter gets subtracted by 1 so you can access the array
    *       with the classical chess notation system.
    * @return an updated column
    */
  def updated(line: Int, piece: Piece): Column =
    new Column(pieces.updated(line - 1, piece))

  /**
    * Saves this column as xml.
    */
  def saveData: IndexedSeq[Elem] = {
    var result: IndexedSeq[Elem] = IndexedSeq()
    pieces.indices.foreach(i => if (pieces(i).nonEmpty) result = result :+ pieces(i).toXml.copy(label = "l" + (i + 1)))
    result
  }

  /**
    * Creates a [[String]] that can be displayed in the console
    */
  override def toString: String = pieces.mkString("| ", " | ", " |")

  override def newBuilder: mutable.Builder[Piece, Column] = Column.newBuilder
}

object Column {

  def apply(pieces: Piece*): Column = fromSeq(pieces)

  def fromSeq(buf: Seq[Piece]): Column = new Column(buf.toArray)

  def newBuilder: mutable.Builder[Piece, Column] = new ArrayBuffer mapResult fromSeq

  implicit def canBuildFrom: CanBuildFrom[Column, Piece, Column] = new CanBuildFrom[Column, Piece, Column] {
    def apply(): mutable.Builder[Piece, Column] = newBuilder

    def apply(from: Column): mutable.Builder[Piece, Column] = newBuilder
  }
}
