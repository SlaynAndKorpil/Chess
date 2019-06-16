package chess.framework.JavaInterfacing;

import chess.framework.Input.*;
import chess.framework.Square;

/**
 * Contains wrapper for inputs for more intuitive use with java.
 * @author Felix Lehner
 * @version alpha 0.1
 */
public class JInput {
    public static MoveParams MoveParams(Square from, Square to) {
        return new MoveParams(from, to);
    }

    public static Promotion Promotion(scala.Function2<chess.framework.AnyColor,java.lang.Object,chess.framework.AnyPiece> pieceGenerator) {
        return new Promotion(pieceGenerator);
    }

    public static Resign$ Resign = Resign$.MODULE$;

    public static DrawOffer$ DrawOffer = DrawOffer$.MODULE$;

    public static DrawReject$ DrawReject = DrawReject$.MODULE$;

    public static DrawAcceptance$ DrawAcceptance = DrawAcceptance$.MODULE$;

    public static TakebackAcceptance$ TakebackAcceptance = TakebackAcceptance$.MODULE$;

    public static TakebackReject$ TakebackReject = TakebackReject$.MODULE$;

    public static TakebackProposal$ TakebackProposal = TakebackProposal$.MODULE$;
}
