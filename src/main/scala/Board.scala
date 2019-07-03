

case class Board(matrix: Board.BoardMatrix) {
  require(matrix.nonEmpty && matrix.head.nonEmpty, "Board should not be empty")
  require(matrix.exists(_.contains(Board.EMPTY_BLOCK)), "Empty block is not specified")
  require(matrix.forall(row => row.size == matrix.head.size), "All rows should have equal length")

  lazy val emptyBlockXY: (Int, Int) = matrix
    .zipWithIndex
    .map({ case (row, y) => (row.indexOf(Board.EMPTY_BLOCK), y) })
    .find({ case (x, y) => x >= 0 })
    .get

  def moveEmptyBlock(move: Move): Option[Board] = {
    val (emptyBlockX, emptyBlockY) = emptyBlockXY
    val boardYBounds = 0 until matrix.length
    val boardXBounds = 0 until matrix.head.length
    val newEmptyBlockX = emptyBlockX + move.x
    val newEmptyBlockY = emptyBlockY + move.y
    if (boardXBounds.contains(newEmptyBlockX) && boardYBounds.contains(newEmptyBlockY)) {
      val newMatrix = if (emptyBlockY != newEmptyBlockY) {
        val sourceRow = matrix(emptyBlockY)
        val destRow = matrix(newEmptyBlockY)
        val newSourceRow = sourceRow.updated(emptyBlockX, destRow(newEmptyBlockX))
        val newDestRow =  destRow.updated(newEmptyBlockX, Board.EMPTY_BLOCK)
        matrix.updated(emptyBlockY, newSourceRow).updated(newEmptyBlockY, newDestRow)
      } else {
        val row = matrix(emptyBlockY)
        val newRow = row.updated(emptyBlockX, row(newEmptyBlockX)).updated(newEmptyBlockX, Board.EMPTY_BLOCK)
        matrix.updated(emptyBlockY, newRow)
      }
      Some(Board(newMatrix))
    } else {
      None
    }
  }
}

object Board extends BoardGenerator {

  type BoardMatrix = Vector[Vector[Int]]

  val EMPTY_BLOCK = 0

  implicit class PrintableBoard(board: Board) extends BoardPrinter {
    lazy val show: String = show(board)
  }

  implicit class ValidatedBoard(board: Board) extends BoardValidator {
    lazy val isValid: Boolean = isValid(board)
  }

}