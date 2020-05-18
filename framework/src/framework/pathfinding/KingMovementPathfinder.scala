package framework.pathfinding

import framework.Sqr

/**
 * Applies the movement of a king to a pathfinder.
 * 
 * If you just stumbled upon this file as you were
 * curious about how the pathfinding works:
 * DONT'T READ ANY FURTHER AND FORGET ANYTHING YOU
 *     ALREADY SAW OF THIS PATHFINDING SYSTEM
 *          (It's for your own good)
 * Just believe it does its work in the most perfect
 * way possible and continue to happily live your life.
 *
 * 
 * If you are here because you had some problems with
 * the false recognition of blocked games as in
 * framework.ChessBoard.isBlocked() (see 
 * https://github.com/SlaynAndKorpil/Chess/issues/85)
 * and are trying to find the error:
 * This whole pathfinding system is an abomination. Not
 * because of its size but because of its sheer unnecessary
 * complexity and unbelievable abstraction paired with an
 * equally astonishing amount of bullshit and inefficiency
 * that makes me wonder about my own sanity every time
 * I think about the fact I created this monstrosity.
 * When I was laying out the foundation of this pile of crap,
 * I was fascinated of the idea of using abstractions
 * to make this code more reusable so I could later
 * reuse it for some other interesting cases of pathfinding
 * in this app. But in the very process of implementing
 * this madness of a plan, I noticed that the most
 * important and fundamental usecase of it - figuring
 * out if a king can capture an enemy piece - got
 * harder and harder to implement in this increasingly
 * convoluted mess. This lead to many changes in
 * my original plan that basically also rendered this
 * abstraction as such completely useless.
 * Now that I know better and learned that OO and its
 * abstractions are a curse in that they can turn any
 * sensible idea of reusability into hundreds of layers
 * of human excrements (so called 'abstraction layers'),
 * I really feel ashamed for this fucked-up piece of code
 * that I gave birth to and that due to
 * its pure evelish nature could only be compared to
 * products such as Windows or the incredible shitshow
 * that is the excessive use of CSS and JS on webpages.
 * I know there are surely many edgecases where it
 * does not operate in any sensible manner which surely
 * does lead to many incorrect or even plain wrong
 * results.
 * 
 * If you are right now thinking about rewriting
 * this mess:
 * IT FUCKING WORKS! I cannot believe this but it does!
 * It is not 100% correct but I personally never had
 * any issues with it and really don't want to touch it again
 * any time soon nor do I even give a shit about your
 * problems.
 *
 * If you now think you are better than me and submit
 * a PR with a fix:
 * I do not care. I will delete any pullrequests that
 * change something in the pathfinding system as 
 * soon as I see it. As this project is under the MIT
 * license I sadly cannot stop you from making your
 * own fork to change it but I can assure you I will
 * make use of my right to not give a shit about
 * your opinion since neither liability nor warrenty
 * is granted.
 *
 * See the 'LICENSE' file or
 * https://github.com/SlaynAndKorpil/Chess/blob/master/LICENSE
 * for detailed description of your rights regarding
 * this project.
 * 
 * ... I really don't know how to feel about that
 * the biggest chunk of documentation in this
 * project is a rant about that I do not want to
 * change this. :thinking:
 * 
 * @author Felix Lehner
 * @version alpha 0.2
 */
abstract class KingMovementPathfinder extends Pathfinder[Boolean] {
  override def success(on: Sqr): Success[Boolean] = Success(true)

  override def continue(from: Sqr): Result[Boolean] = apply(from)

  override def apply(pos: Sqr): Result[Boolean] = {
    val agents = Array(
      KingMovement(0, 1), KingMovement(0, -1),
      KingMovement(1, 1), KingMovement(1, 0), KingMovement(1, -1),
      KingMovement(-1, 1), KingMovement(-1, 0), KingMovement(-1, -1),
    )

    val results: Array[Result[Boolean]] =
      agents
        .map(pathfinder => TerminableOnce[Boolean](pathfinder))
        .map(terminableOnce => terminableOnce.apply(pos + terminableOnce.pathfinder.vector))

    results find (_.isSuccess) match {
      case Some(success) => success
      case None => Failure
    }
  }

  override def terminate(on: Sqr): Failure.type = Failure

  override def decision(pos: Sqr): WaypointResult.Value

  private case class KingMovement(override val vector: (Int, Int))
    extends TripleDirectionalPathfinder[Boolean](vector) with BooleanPathfinder {
    override def decision(pos: Sqr): WaypointResult.Value = KingMovementPathfinder.this.decision(pos)
  }
}
