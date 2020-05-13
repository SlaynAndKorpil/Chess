# Chess
This project is mostly for experimental and testing purpose as it is my first project on github.
Don't take it too seriously. (Though it works pretty well, and I appreciate any support)

## Components
 0. A backend.
 0. An GUI implementation of the backend.
 0. A CLI to the backend.

## [Backend](https://github.com/SlaynAndKorpil/Chess/tree/master/framework)
A Framework used to simulate a chess game and capable of storing and computing positions, moves, loading and saving.
When using scala, you can interact with it through [framework.ChessIO](https://github.com/SlaynAndKorpil/Chess/blob/master/framework/src/framework/ChessIO.scala) trait as a pipe for in&output or [JChessIO](
https://github.com/SlaynAndKorpil/Chess/blob/master/framework/src/framework/javaInterfacing/JChessIO.java) for use with Java.
The main game logic is located in [framework.ChessBoard](https://github.com/SlaynAndKorpil/Chess/blob/master/framework/src/framework/ChessBoard.scala).
It currently features:
   0. Recognition and execution of correct moves
   0. Promotions
   0. Any types of draws (insufficient material, stalemate, ...) and mates
   0. Saving and loading your games
   0. Currently no AI opponent but I might be adding Stockfish or Lila when I feel like it
   0. A full java interface

## [GUI Application](https://github.com/SlaynAndKorpil/Chess/tree/master/graphics)
When you start the .jar the game will open.
Right now there are neither animations nor sounds because I normally turn these off in chess apps. I might add these in on demand.

## [CLI Application](https://github.com/SlaynAndKorpil/Chess/tree/master/console)
Use 'console' argument to start in console mode:
> java -jar Chess.jar console
