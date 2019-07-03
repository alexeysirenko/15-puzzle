trait BoardValidator {
  def isValid(board: Board): Boolean = {
    val mergedRows = board.matrix.reduce(_ ++ _)
    val withoutLastDice = mergedRows.dropRight(1)
    mergedRows.last == Board.EMPTY_DICE && withoutLastDice == withoutLastDice.sorted
  }
}
