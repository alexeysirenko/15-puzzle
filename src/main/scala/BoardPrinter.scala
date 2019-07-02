trait BoardPrinter {
  def show(board: Board): String = {
    val biggestDice = board.state.reduce(_ ++ _).sortWith(_ > _).head
    val diceSize = biggestDice.toString.length
    val stringBuilder = new StringBuilder
    board.state.foreach { row =>
      val printableRow = row
        .map(value => if (value == Board.emptyBlock) " " else value.toString)
        .map(value => " " * (diceSize - value.length) + value)
        .mkString("|", "|", "|")
      stringBuilder.append(printableRow + "\n")
    }
    stringBuilder.toString()
  }
}
