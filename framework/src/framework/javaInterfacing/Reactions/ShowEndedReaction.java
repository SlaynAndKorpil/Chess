package framework.javaInterfacing.Reactions;

import framework.IOEvents.IOEvent;
import framework.IOEvents.ShowEnded;

import java.util.function.Consumer;

public class ShowEndedReaction extends JReaction<ShowEnded> {
    public boolean isDefinedAt(IOEvent event) {
        return event.getClass() == ShowEnded.class;
    }

    public ShowEndedReaction(Consumer<ShowEnded> reaction) {
        this.reaction = reaction;
    }
}
