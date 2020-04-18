package framework.javaInterfacing.Reactions;

import framework.IOEvents.IOEvent;
import framework.IOEvents.NoEvent$;

import java.util.function.Consumer;

public class ReactToAll extends JReaction<IOEvent> {
    public boolean isDefinedAt(IOEvent event) {
        return event.getClass() != NoEvent$.class;
    }
    public ReactToAll(Consumer<IOEvent> reaction) {
        this.reaction = reaction;
    }
}
