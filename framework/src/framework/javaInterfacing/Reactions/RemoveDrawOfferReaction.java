package framework.javaInterfacing.Reactions;

import framework.IOEvents.IOEvent;
import framework.IOEvents.RemoveDrawOffer$;

import java.util.function.*;

public class RemoveDrawOfferReaction extends JReaction<RemoveDrawOffer$> {
    public RemoveDrawOfferReaction(Consumer<RemoveDrawOffer$> reaction) {
        this.reaction = reaction;
    }
    public boolean isDefinedAt(IOEvent event) {
        return event.getClass() == RemoveDrawOffer$.class;
    }
}
