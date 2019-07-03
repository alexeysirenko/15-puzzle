import scala.util.Random

object Application extends App {

  val BOARD_SIZE = 1

  val controls = Map(
    "w" -> (Puzzle.moveUp, "move up"),
    "s" -> (Puzzle.moveDown, "move down"),
    "a" -> (Puzzle.moveLeft, "move left"),
    "d" -> (Puzzle.moveRight, "move right")
  )

  println("Controls:")
  println(showControls(controls))

  var board = Board.createBoard(Random.nextLong(), BOARD_SIZE)

  while (true) {
    println(board.show)
    if (board.isValid) println("You have won!")
    val input = scala.io.StdIn.readLine()
    board = controls.get(input) match {
      case Some((move, _)) => Puzzle.update(move)(board)
      case _ =>
        println(s"Unknown command '$input'")
        board
    }
  }

  def showControls(controls: Map[String, (Move, String)]): String = controls
    .foldLeft("")({ case (acc, (key, (_, description))) =>
      s"$acc$key - $description\n"
    })
}
