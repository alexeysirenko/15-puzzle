trait BoardValidator {
  def isValid(board: Board): Boolean = {
    val mergedRows = board.state.reduce(_ ++ _)
    val withoutLastDice = mergedRows.dropRight(1)
    mergedRows.last == Board.emptyBlock && withoutLastDice == withoutLastDice.sorted
  }
}
