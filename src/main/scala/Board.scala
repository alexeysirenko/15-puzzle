
case class Board(state: Vector[Vector[Int]]) { // TODO: create a type alias for this
  require(state.nonEmpty && state.head.nonEmpty, "Board should not be empty")
  require(state.exists(_.contains(Board.emptyBlock)), "Empty block is not specified")

  lazy val emptyBlockXY: (Int, Int) = {
    state
      .zipWithIndex
      .map({ case (row, y) => (row.indexOf(Board.emptyBlock), y) })
      .find({ case (x, y) => x >= 0 })
      .get
  }

  def moveEmptyBlock(move: Move): Option[Board] = {
    val (emptyBlockX, emptyBlockY) = emptyBlockXY
    val boardYBounds = 0 until state.length
    val boardXBounds = 0 until state.head.length
    val newEmptyBlockX = emptyBlockX + move.x
    val newEmptyBlockY = emptyBlockY + move.y
    if (boardXBounds.contains(newEmptyBlockX) && boardYBounds.contains(newEmptyBlockY)) {
      val newState = if (emptyBlockY != newEmptyBlockY) {
        val sourceRow = state(emptyBlockY)
        val destRow = state(newEmptyBlockY)
        val newSourceRow = sourceRow.updated(emptyBlockX, destRow(newEmptyBlockX))
        val newDestRow =  destRow.updated(newEmptyBlockX, Board.emptyBlock)
        state.updated(emptyBlockY, newSourceRow).updated(newEmptyBlockY, newDestRow)
      } else {
        val row = state(emptyBlockY)
        val newRow = row.updated(emptyBlockX, row(newEmptyBlockX)).updated(newEmptyBlockX, Board.emptyBlock)
        state.updated(emptyBlockY, newRow)
      }
      Some(Board(newState))
    } else {
      None
    }
  }
}

object Board extends BoardGenerator {

  val emptyBlock = 0

  // TODO: move to tests
  def init2(seed: Int): Board = {
    // todo: generate randomly
    val l1 = Vector(1, 2, 3, 4)
    val l2 = Vector(5, 6, 7, 8)
    val l3 = Vector(9, 10, 11, 12)
    val l4 = Vector(13, 14, emptyBlock, 15)

    Board(Vector(l1, l2, l3, l4))
  }

  implicit class PrintableBoard(board: Board) extends BoardPrinter {
    lazy val show: String = show(board)
  }

  implicit class ValidatedBoard(board: Board) extends BoardValidator {
    lazy val isValid: Boolean = isValid(board)
  }

}