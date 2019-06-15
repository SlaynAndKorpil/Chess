package chess.framework.JavaInterfacing;

import chess.framework.ChessBoard;
import chess.framework.ChessIO;
import chess.framework.IOEvents.BoardReactions;
import chess.framework.IOEvents.IOEvent;
import chess.framework.IOEvents.JReaction;
import chess.framework.Input.Input;
import chess.framework.Output;
import scala.collection.IndexedSeq;

/**
 * A wrapper for the scala version of this interface.
 * I literally spent hours on trying to build this as a real interface
 * with the same functionality and user-friendliness... Java is BS!
 *
 * @version alpha 0.1
 * @author Felix Lehner
 */
@SuppressWarnings("ALL")
public abstract class JChessIO {
    protected ChessIO wrappedRef = new ChessIO() {
        public void board_$eq(ChessBoard board) {
            JChessIO.this.setChessBoard(board);
        }

        //no idea why the compiler doesn't get these are vals?!?
        public void chess$framework$ChessIO$_setter_$chessReactions_$eq(BoardReactions reactions) {chessReactions = reactions;}
        public void chess$framework$ChessIO$_setter_$io_$eq(chess.framework.ChessIO io) {}

        public ChessBoard board() {
            return JChessIO.this.getChessBoard();
        }

        public void update() {
            JChessIO.this.update();
        }

        private void reactTo(IndexedSeq<IOEvent> events) {
            if(!events.isEmpty()) {
                chessReactions().apply(events.head());
                reactTo(events.tail().toIndexedSeq());
            }
        }

        protected BoardReactions chessReactions = new BoardReactions();

        public void receiveInput(Input<?> input) {
            Output res = (Output) board().receive(input).getOrElse(null);
            if (res != null) {
                setChessBoard(res.board());
                update();
                reactTo(res.events());
            }
        }

        public BoardReactions chessReactions() {
            return chessReactions;
        }

        public ChessIO io() {
            return this;
        }
    };

    public ChessBoard getChessBoard() {
        return chessBoard;
    }

    public void setChessBoard(ChessBoard chessBoard) {
        this.chessBoard = chessBoard;
    }

    protected ChessBoard chessBoard;

    protected abstract void update();

    protected void giveInput(Input<?> input) {
        wrappedRef.receiveInput(input);
    }

    protected void addReaction(JReaction reaction) {
        wrappedRef.chessReactions().add(reaction);
    }
}
