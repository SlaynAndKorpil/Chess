package chess.framework.JavaInterfacing;

import chess.framework.BoardStatus.GameResult.GameResult;
import chess.framework.IOEvents.*;
import chess.framework.Square;

/**
 * Provides wrappers for IOEvents.
 * @author Felix Lehner
 * @version alpha 0.1
 */
public class JEvents {
    public static ShowDrawOffer$ ShowDrawOffer = ShowDrawOffer$.MODULE$;

    public static RemoveDrawOffer$ RemoveDrawOffer = RemoveDrawOffer$.MODULE$;

    public static ShowPromotion ShowPromotion(Square on) {
        return new ShowPromotion(on);
    }

    public static RemovePromotion$ RemovePromotion = RemovePromotion$.MODULE$;

    public static ShowTakeback$ ShowTakeback = ShowTakeback$.MODULE$;

    public static RemoveTakeback$ RemoveTakeback = RemoveTakeback$.MODULE$;

    public static chess.framework.IOEvents.ShowEnded ShowEnded(GameResult result) {
        return new chess.framework.IOEvents.ShowEnded(result);
    }

    public static ShowCheck ShowCheck(Square on) {
        return new ShowCheck(on);
    }

    public static NoEvent$ NoEvent = NoEvent$.MODULE$;
}
