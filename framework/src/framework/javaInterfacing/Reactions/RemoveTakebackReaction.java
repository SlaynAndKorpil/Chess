package framework.javaInterfacing.Reactions;

import framework.IOEvents.IOEvent;
import framework.IOEvents.RemoveTakeback$;

import java.util.function.Consumer;

public class RemoveTakebackReaction extends JReaction<RemoveTakeback$> {
    public boolean isDefinedAt(IOEvent event) {
        return event.getClass() == RemoveTakeback$.class;
    }

    public RemoveTakebackReaction(Consumer<RemoveTakeback$> reaction) {
        this.reaction = reaction;
    }
}
