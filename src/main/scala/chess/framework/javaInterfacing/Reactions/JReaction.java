package chess.framework.javaInterfacing.Reactions;

import chess.framework.IOEvents.IOEvent;

import java.util.function.Consumer;

/**
 * A java version of Reaction for convenience.
 * This takes a Consumer as reaction for interoperability with java.
 * @param <T> some event type
 * @author Felix Lehner
 * @version alpha 0.1
 */
public abstract class JReaction<T extends IOEvent> {
    public abstract boolean isDefinedAt(IOEvent event);
    public Consumer<T> reaction;
}
