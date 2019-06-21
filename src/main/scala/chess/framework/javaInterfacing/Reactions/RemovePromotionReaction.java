package chess.framework.javaInterfacing.Reactions;

import chess.framework.IOEvents.IOEvent;
import chess.framework.IOEvents.RemovePromotion$;

import java.util.function.Consumer;

public class RemovePromotionReaction extends JReaction<RemovePromotion$> {
    public boolean isDefinedAt(IOEvent event) {
        return event.getClass() == RemovePromotion$.class;
    }

    public RemovePromotionReaction(Consumer<RemovePromotion$> reaction) {
        this.reaction = reaction;
    }
}
