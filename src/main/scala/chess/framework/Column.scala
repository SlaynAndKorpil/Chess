package chess.framework

import scala.collection.generic.CanBuildFrom
import scala.collection.mutable.ArrayBuffer
import scala.collection.{IndexedSeqLike, mutable}
import scala.language.postfixOps
import scala.xml.{Elem, NodeSeq}

final class Column extends IndexedSeq[Piece] with IndexedSeqLike[Piece, Column] {
  val length = 8

  private var pieces: Array[Piece] = Array.fill(8) (NoPiece)

  def this (ps: Map[Int, Piece]) = {
    this()
    ps foreach (line => if(line._1 > 0 && line._1 < 9) pieces = pieces.updated(line._1-1, line._2))
  }

  def this (ps: Seq[Piece]) = {
    this()
    if(ps.length == 8)
     ps.indices foreach (lineNr => pieces.update(lineNr, ps(lineNr)))
  }

  def this (piece: Piece) = {
    this()
    pieces = Array.fill(8)(piece)
  }

  def apply (idx: Int): Piece = pieces(idx-1)

  def updated(line: Int, piece: Piece): Column = new Column(pieces.updated(line-1, piece))

  def updated(line: Int, piece: Char, color: AnyColor): Column = updated(line, piece match {
    case 'P' => Pawn(color)
    case 'R' => Rook(color)
    case 'N' => Knight(color)
    case 'B' => Bishop(color)
    case 'K' => King(color)
    case 'Q' => Queen(color)
    case _ => NoPiece
  })


  def saveData: IndexedSeq[Elem] = {
    var result: IndexedSeq[Elem] = IndexedSeq()
    pieces.indices.foreach (i => if (pieces(i).nonEmpty) result = result :+ pieces(i).xml.copy(label = "l"+(i+1)))
    result
  }

  override def toString: String = pieces.mkString ("|", "|", "|")


  override def newBuilder: mutable.Builder[Piece, Column] = Column.newBuilder
}

object Column {
  def fromSeq (buf: Seq[Piece]): Column = new Column(buf)

  def apply (pieces: Piece*): Column = fromSeq(pieces)

  def newBuilder: mutable.Builder[Piece, Column] = new ArrayBuffer mapResult fromSeq

  implicit def canBuildFrom: CanBuildFrom[Column, Piece, Column] = new CanBuildFrom[Column, Piece, Column] {
    def apply(): mutable.Builder[Piece, Column] = newBuilder
    def apply(from: Column): mutable.Builder[Piece, Column] = newBuilder
  }

  def loadFromXML (xml: NodeSeq): Column = {
    //TODO rewrite in functional style
    var result = new Column(NoPiece)
    for (i <- 1 to 8; label = "l"+i; if (xml \ label) != NodeSeq.Empty; data = xml \ label)
      result = result.updated(i, Piece((data \ "id").text.filter(c => c != ' ' && c != '\n').head, Color((data \ "color").text.filter(c => c != ' ' && c != '\n')), (data \ "moved").text.filter(c => c != ' ' && c != '\n').toBoolean))
    result
  }
}
