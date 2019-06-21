package chess.framework.javaInterfacing.Reactions;

import chess.framework.IOEvents.IOEvent;
import chess.framework.IOEvents.ShowEnded;

import java.util.function.Consumer;

public class ShowEndedReaction extends JReaction<ShowEnded> {
    public boolean isDefinedAt(IOEvent event) {
        return event.getClass() == ShowEnded.class;
    }

    public ShowEndedReaction(Consumer<ShowEnded> reaction) {
        this.reaction = reaction;
    }
}
