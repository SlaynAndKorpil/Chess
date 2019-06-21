package chess.framework.javaInterfacing.Reactions;

import chess.framework.IOEvents.IOEvent;
import chess.framework.IOEvents.RemoveTakeback$;

import java.util.function.Consumer;

public class RemoveTakebackReaction extends JReaction<RemoveTakeback$> {
    public boolean isDefinedAt(IOEvent event) {
        return event.getClass() == RemoveTakeback$.class;
    }

    public RemoveTakebackReaction(Consumer<RemoveTakeback$> reaction) {
        this.reaction = reaction;
    }
}
