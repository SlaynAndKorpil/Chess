package framework.javaInterfacing.Reactions;

import framework.IOEvents.IOEvent;
import framework.IOEvents.ShowCheck;

import java.util.function.Consumer;

public class ShowCheckReaction extends JReaction<ShowCheck> {
    public boolean isDefinedAt(IOEvent event) {
        return event.getClass() == ShowCheck.class;
    }

    public ShowCheckReaction(Consumer<ShowCheck> reaction) {
        this.reaction = reaction;
    }
}
