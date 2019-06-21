package chess.framework.javaInterfacing.Reactions;

import chess.framework.IOEvents.IOEvent;
import chess.framework.IOEvents.ShowPromotion;

import java.util.function.Consumer;

public class ShowPromotionReaction extends JReaction<ShowPromotion> {
    public boolean isDefinedAt(IOEvent event) {
        return event.getClass() == ShowPromotion.class;
    }

    public ShowPromotionReaction(Consumer<ShowPromotion> reaction) {
        this.reaction = reaction;
    }
}
