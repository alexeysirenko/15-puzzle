trait Input
case class Move(x: Int, y: Int) extends Input

object Puzzle {

  val moveUp = Move(x = 0, y = -1)
  val moveDown = Move(x = 0, y = 1)
  val moveLeft = Move(x = -1, y = 0)
  val moveRight = Move(x = 1, y = 0)

  def update: Input => Board => Board = (i: Input) => (b: Board) =>
    (i, b) match {
      case (move @ Move(_, _), board) => board.moveEmptyDice(move).getOrElse(board)
      case _ => b
    }
}
