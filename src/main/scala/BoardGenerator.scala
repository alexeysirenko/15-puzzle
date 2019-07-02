trait BoardGenerator {

  def createBoard(randomSeed: Long, size: Int): Board = {
    val initialBoardState = (Board.emptyBlock :: (1 until size * size)
      .foldLeft(List.empty[Int])((acc, value) => value :: acc))
      .reverse
      .toVector

    val (randomizedBoardState, _) = (0 until initialBoardState.length)
      .foldLeft((initialBoardState, SimpleRNG(randomSeed): RNG)) { case ((boardState, rng), index) =>
        val (randomIndex, newRNG) = RNG.nonNegativeLessThan(boardState.length)(rng)
        val oldValueByIndex = boardState(index)
        val oldValueByRandomIndex = boardState(randomIndex)
        (boardState.updated(index, oldValueByRandomIndex).updated(randomIndex, oldValueByIndex), newRNG)
      }

    Board(randomizedBoardState.sliding(size, size).toVector)
  }

}
