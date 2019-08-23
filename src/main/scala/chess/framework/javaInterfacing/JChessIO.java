package chess.framework.javaInterfacing;

import chess.framework.ChessBoard;
import chess.framework.ChessBoard$;
import chess.framework.ChessIO;
import chess.framework.FileOperationError.FileNotFoundError;
import chess.framework.FileOperationError.FileOperationError;
import chess.framework.IOEvents.BoardReactions;
import chess.framework.IOEvents.IOEvent;
import chess.framework.IOEvents.ShowCheck;
import chess.framework.Input.Input;
import chess.framework.Output;
import chess.framework.javaInterfacing.Reactions.JReaction;
import scala.None$;
import scala.Option;
import scala.Some;
import scala.collection.IndexedSeq;
import scala.util.Either;

import java.util.function.Function;

/**
 * A wrapper for the scala version of this interface.
 * I literaly spent hours on trying to build this as a real interface
 * with the same functionality and user-friendliness... Java is BS!
 *
 * @author Felix Lehner
 * @version alpha 0.3
 */
@SuppressWarnings("ALL")
public abstract class JChessIO {
    /**
     * Initiallizes with a classic chess board.
     */
    public JChessIO() {
        this(ChessBoard$.MODULE$::classicalBoard);
    }

    /**
     * Initializes the chessBoard as well as the wrappedRef.
     *
     * @param func a generator for chessboards (e.g. <code>ChessBoard$.MODULE$::classicalBoard</code>)
     */
    protected JChessIO(Function<ChessIO, ChessBoard> func) {
        wrappedRef = new ChessIO() {
            @Override
            public void lastSavePath_$eq(String lastSavePath) {
                setLastSavePath(lastSavePath);
            }

            @Override
            public Option<FileOperationError> load(String filePath) {
                Either<chess.framework.FileOperationError.FileOperationError, ChessBoard> loadRes =
                        ChessBoard$.MODULE$.load(filePath, io());
                if (loadRes.isRight()) {
                    ChessBoard loadedBoard = loadRes.right().get();
                    setChessBoard(loadedBoard);
                    loadedBoard.doOnCheck(
                            pos -> {
                                chessReactions.apply(ShowCheck.apply(pos));
                                return "";
                            },
                            "",
                            getChessBoard().turn());
                    return None$.empty();
                } else {
                    return new Some(loadRes.left().get());
                }
            }

            @Override
            public ChessBoard chessBoard() {
                return getChessBoard();
            }

            @Override
            public void chessBoard_$eq(ChessBoard board) {
                setChessBoard(board);
            }

            @Override
            public Option<FileNotFoundError> save(String filePath) {
                if (filePath != null && filePath != "" && getLastSavePath() != filePath) setLastSavePath(filePath);
                return ChessBoard$.MODULE$.save(getChessBoard(), getLastSavePath());
            }

            @Override
            public String lastSavePath() {
                return getLastSavePath();
            }

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
     * ChessBoard board = ChessBoard$.MODULE$.classicalBoard(getWrappedRef())
     * setChessBoard(board);
     * </code>
     */
    public ChessIO getWrappedRef() {
        return wrappedRef;
    }

    private ChessIO wrappedRef;

    public ChessBoard getChessBoard() {
        return chessBoard;
    }

    public void setChessBoard(ChessBoard chessBoard) {
        this.chessBoard = chessBoard;
        update();
    }

    protected ChessBoard chessBoard;


    public String getLastSavePath() {
        return lastSavePath;
    }

    public void setLastSavePath(String lastSavePath) {
        this.lastSavePath = lastSavePath;
    }

    protected String lastSavePath;

    /**
     * This method should update the output (e.g a GUI) and reload the data
     * from the board into the data structure you are using.
     * <p>
     * This should always clear any visual indication of a check as there is no event for this
     * because every king checked won't be checked after the next move (i.e. no legal move
     * of a checked player will ever result in being checked again.
     * <p>
     * This method gets called after every change of the board.
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

    /**
     * Saves the current game to a file.
     *
     * @param filePath The file the game used to store the data.
     *                 {@code .save} is added when there is no file extension yet.
     */
    protected void save(String filePath) throws FileNotFoundError {
        Option<chess.framework.FileOperationError.FileNotFoundError> errorOpt = wrappedRef.save(filePath);
        if (errorOpt.isDefined()) throw errorOpt.get();
    }

    /**
     * Loads a saved game from a file.
     *
     * @param filePath The path to the file. If it does not contain a file
     *                 extension, {@code .save} is added to it.
     * @throws FileOperationError when an error occurs whilest parsing
     */
    protected void load(String filePath) throws FileOperationError {
        scala.Option<FileOperationError> result = wrappedRef.load(filePath);
        if (result.isDefined()) throw result.get();
    }
}
