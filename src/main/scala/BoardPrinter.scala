trait BoardPrinter {

  def show(board: Board): String = {
    val biggestDice = board.matrix.reduce(_ ++ _).sortWith(_ > _).head
    val diceSize = biggestDice.toString.length

    board.matrix.foldLeft("")({ (acc, row) =>
      val printableRow = row
        .map({value =>
          val diceTitle = if (value == Board.EMPTY_DICE) " " else value.toString
          val diceTitleWithPadding = " " * (diceSize - diceTitle.length) + diceTitle
          diceTitleWithPadding
        }).mkString("|", "|", "|")
      s"$acc$printableRow\n"
    })
  }
}
