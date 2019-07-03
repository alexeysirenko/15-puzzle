

case class Board(matrix: Board.BoardMatrix) {
  require(matrix.nonEmpty && matrix.head.nonEmpty, "Board should not be empty")
  require(matrix.exists(_.contains(Board.EMPTY_DICE)), "Empty Dice is not specified")
  require(matrix.forall(row => row.size == matrix.head.size), "All rows should have equal length")

  lazy val emptyDiceXY: (Int, Int) = matrix
    .zipWithIndex
    .map({ case (row, y) => (row.indexOf(Board.EMPTY_DICE), y) })
    .find({ case (x, y) => x >= 0 })
    .get

  def moveEmptyDice(move: Move): Option[Board] = {
    val (emptyDiceX, emptyDiceY) = emptyDiceXY
    val boardYBounds = 0 until matrix.length
    val boardXBounds = 0 until matrix.head.length
    val newEmptyDiceX = emptyDiceX + move.x
    val newEmptyDiceY = emptyDiceY + move.y
    val isNewDiceWithinBounds = boardXBounds.contains(newEmptyDiceX) && boardYBounds.contains(newEmptyDiceY)
    if (isNewDiceWithinBounds) {
      val moveVertically = emptyDiceY != newEmptyDiceY
      val newMatrix = if (moveVertically) {
        val sourceRow = matrix(emptyDiceY)
        val destRow = matrix(newEmptyDiceY)
        val newSourceRow = sourceRow.updated(emptyDiceX, destRow(newEmptyDiceX))
        val newDestRow =  destRow.updated(newEmptyDiceX, Board.EMPTY_DICE)
        matrix.updated(emptyDiceY, newSourceRow).updated(newEmptyDiceY, newDestRow)
      } else {
        val row = matrix(emptyDiceY)
        val newRow = row.updated(emptyDiceX, row(newEmptyDiceX)).updated(newEmptyDiceX, Board.EMPTY_DICE)
        matrix.updated(emptyDiceY, newRow)
      }
      Some(Board(newMatrix))
    } else {
      None
    }
  }
}

object Board extends BoardGenerator {

  type BoardMatrix = Vector[Vector[Int]]

  val EMPTY_DICE = 0

  implicit class PrintableBoard(board: Board) extends BoardPrinter {
    lazy val show: String = show(board)
  }

  implicit class ValidatedBoard(board: Board) extends BoardValidator {
    lazy val isValid: Boolean = isValid(board)
  }

}