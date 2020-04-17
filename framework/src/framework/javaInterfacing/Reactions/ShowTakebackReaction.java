package framework.javaInterfacing.Reactions;

import framework.IOEvents.IOEvent;
import framework.IOEvents.ShowTakeback$;

import java.util.function.Consumer;

public class ShowTakebackReaction extends JReaction<ShowTakeback$> {
    public boolean isDefinedAt(IOEvent event) {
        return event.getClass() == ShowTakeback$.class;
    }

    public ShowTakebackReaction(Consumer<ShowTakeback$> reaction) {
        this.reaction = reaction;
    }
}
