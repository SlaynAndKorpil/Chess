package framework.javaInterfacing.Reactions;

import framework.IOEvents.IOEvent;
import framework.IOEvents.RemovePromotion$;

import java.util.function.Consumer;

public class RemovePromotionReaction extends JReaction<RemovePromotion$> {
    public boolean isDefinedAt(IOEvent event) {
        return event.getClass() == RemovePromotion$.class;
    }

    public RemovePromotionReaction(Consumer<RemovePromotion$> reaction) {
        this.reaction = reaction;
    }
}
