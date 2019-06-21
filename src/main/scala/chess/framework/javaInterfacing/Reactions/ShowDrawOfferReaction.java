package chess.framework.javaInterfacing.Reactions;

import chess.framework.IOEvents.IOEvent;
import chess.framework.IOEvents.ShowDrawOffer$;

import java.util.function.Consumer;

public class ShowDrawOfferReaction extends JReaction<ShowDrawOffer$> {
    public ShowDrawOfferReaction(Consumer<ShowDrawOffer$> reaction) {
        this.reaction = reaction;
    }

    public boolean isDefinedAt(IOEvent event) {
        return event.getClass() == ShowDrawOffer$.class;
    }
}
