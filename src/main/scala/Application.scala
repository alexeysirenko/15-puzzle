import scala.util.Random

object Application extends App {

  println("Controls:")
  println("w - move up")
  println("s - move down")
  println("a - move left")
  println("d - move right")
  println("Ctrl + C - exit the game")

  var board = Board.createBoard(Random.nextLong(), 4)

  while (true) {
    println(board.show)
    if (board.isValid) println("You have won!")
    board = scala.io.StdIn.readLine() match {
      case "w" => Puzzle.update(Puzzle.moveUp)(board)
      case "s" => Puzzle.update(Puzzle.moveDown)(board)
      case "a" => Puzzle.update(Puzzle.moveLeft)(board)
      case "d" => Puzzle.update(Puzzle.moveRight)(board)
      case unknown =>
        println(s"Unknown command $unknown")
        board
    }
  }


}
