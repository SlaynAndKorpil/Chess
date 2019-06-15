package chess.framework.JavaInterfacing;

import chess.framework.ChessBoard;
import chess.framework.ChessIO;
import chess.framework.IOEvents.BoardReactions;
import chess.framework.IOEvents.IOEvent;
import chess.framework.Input.Input;
import chess.framework.Output;
import scala.collection.IndexedSeq;

import java.util.function.Function;

/**
 * A wrapper for the scala version of this interface.
 * I literally spent hours on trying to build this as a real interface
 * with the same functionality and user-friendliness... Java is BS!
 *
 * @author Felix Lehner
 * @version alpha 0.1
 */
@SuppressWarnings("ALL")
public abstract class JChessIO {
    /**
     * Initializes the chessBoard as well as the wrappedRef.
     *
     * @param func a generator for chessboards (e.g. <code>ChessBoard$.MODULE$::classicalBoard</code>)
     */
    public JChessIO(Function<ChessIO, ChessBoard> func) {
        wrappedRef = new ChessIO() {
            public void board_$eq(ChessBoard board) {
                JChessIO.this.setChessBoard(board);
            }

            //no idea why the compiler doesn't get these are vals?!?
            public void chess$framework$ChessIO$_setter_$chessReactions_$eq(BoardReactions reactions) {
                chessReactions = reactions;
            }

            public void chess$framework$ChessIO$_setter_$io_$eq(chess.framework.ChessIO io) {
            }

            public ChessBoard board() {
                return JChessIO.this.getChessBoard();
            }

            public void update() {
                JChessIO.this.update();
            }

            private void reactTo(IndexedSeq<IOEvent> events) {
                if (!events.isEmpty()) {
                    chessReactions().apply(events.head());
                    reactTo(events.tail().toIndexedSeq());
                }
            }

            protected BoardReactions chessReactions = new BoardReactions();

            public void receiveInput(Input<?> input) {
                Output res = (Output) board().receive(input).getOrElse(null);
                if (res != null) {
                    setChessBoard(res.board());
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

        chessBoard = func.apply(wrappedRef);
    }

    /**
     * A reference to the scala ChessIO because initializing ChessBoards requires one.
     * This also redirects any events and calls to update() to the JChessIO.
     * <p>
     * Use example:
     * <p>
     * <code>
     * import chess.framework.ChessBoard$;
     * ChessBoard board = ChessBoard$.MODULE$.classicalBoard(wrappedRef)
     * setChessBoard(board);
     * </code>
     */
    protected ChessIO wrappedRef;

    public ChessBoard getChessBoard() {
        return chessBoard;
    }

    public void setChessBoard(ChessBoard chessBoard) {
        this.chessBoard = chessBoard;
        update();
    }

    protected ChessBoard chessBoard;

    /**
     * This method should update the output (e.g a GUI) and reload the data
     * from the board into the data structure you are using.
     * <p>
     * This should always clear any visual indication of a check as there is no event for this
     * because every king checked won't be checked after the next move (i.e. no legal move
     * of a checked player will ever result in being checked again.
     * <p>
     * This method gets called by the `receiveInput` method after every change of the board.
     */
    protected abstract void update();

    protected void giveInput(Input<?> input) {
        wrappedRef.receiveInput(input);
    }

    /**
     * Adds a reaction to the event handler.
     */
    protected void addReaction(JReaction reaction) {
        wrappedRef.chessReactions().add(reaction);
    }
}
