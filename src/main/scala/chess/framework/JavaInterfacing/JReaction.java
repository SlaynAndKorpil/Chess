package chess.framework.JavaInterfacing;

import chess.framework.IOEvents.IOEvent;

import java.util.function.*;

/**
 * A java version of Reaction.
 * This takes a Consumer as reaction for interoperability with java.
 * @param <T> some event type
 * @author Felix Lehner
 * @version alpha 0.1
 */
public class JReaction<T extends IOEvent> {
    public JReaction (Consumer<T> reaction) {
        this.reaction = reaction;
    }
    public final Consumer<T> reaction;
}
